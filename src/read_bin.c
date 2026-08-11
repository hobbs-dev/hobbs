#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

// hobbs binary record layout, little endian as written by Rust:
// header, then repeated records:
//   u64 iter, u8 accepted, 7 bytes padding, f64 logp, f64[dim] theta
// Header layout for current files:
//   magic bytes
//   u64 version
//   u64 dim
//   u64 total_iters
//   u64 expected_saved
//   u64 record_size
//
// Current hobbs files use hobbs_BIN_V1 padded to 16 bytes: 56-byte total header.
// Older FASTAMH_BIN_V1 16-byte headers are also supported for compatibility.
// Headerless files are supported when dim is supplied.

static const unsigned char hobbs_MAGIC_16[16] = {
    'X','M','C','M','C','_','B','I','N','_','V','1','\0','\0','\0','\0'
};

static const unsigned char FASTAMH_MAGIC_16[16] = {
    'F','A','S','T','A','M','H','_','B','I','N','_','V','1','\0','\0'
};

static double read_f64_le(const unsigned char* p) {
    uint64_t u = ((uint64_t)p[0]) |
                 ((uint64_t)p[1] << 8) |
                 ((uint64_t)p[2] << 16) |
                 ((uint64_t)p[3] << 24) |
                 ((uint64_t)p[4] << 32) |
                 ((uint64_t)p[5] << 40) |
                 ((uint64_t)p[6] << 48) |
                 ((uint64_t)p[7] << 56);
    double x;
    memcpy(&x, &u, 8);
    return x;
}

static uint64_t read_u64_le(const unsigned char* p) {
    return ((uint64_t)p[0]) |
           ((uint64_t)p[1] << 8) |
           ((uint64_t)p[2] << 16) |
           ((uint64_t)p[3] << 24) |
           ((uint64_t)p[4] << 32) |
           ((uint64_t)p[5] << 40) |
           ((uint64_t)p[6] << 48) |
           ((uint64_t)p[7] << 56);
}

SEXP hobbs_read_bin(SEXP path_s, SEXP dim_s, SEXP max_records_s) {
    if (!Rf_isString(path_s) || LENGTH(path_s) != 1) Rf_error("path must be a string");
    if (!Rf_isInteger(dim_s) || LENGTH(dim_s) != 1) Rf_error("dim must be an integer or NULL");
    if (!Rf_isInteger(max_records_s) || LENGTH(max_records_s) != 1) Rf_error("max_records must be an integer or NULL");

    int dim_requested = INTEGER(dim_s)[0];
    int max_records = INTEGER(max_records_s)[0];
    if (dim_requested == 0 || dim_requested < -1) Rf_error("dim must be positive, or NULL to infer from header");

    const char* path = CHAR(STRING_ELT(path_s, 0));
    FILE* fp = fopen(path, "rb");
    if (!fp) Rf_error("could not open file");

    if (fseek(fp, 0, SEEK_END) != 0) { fclose(fp); Rf_error("seek failed"); }
    long long bytes_ll = (long long)ftell(fp);
    if (bytes_ll < 0) { fclose(fp); Rf_error("ftell failed"); }
    size_t bytes = (size_t)bytes_ll;
    rewind(fp);

    size_t header_size = 0;
    size_t rec_size = 0;
    int dim = dim_requested;

    unsigned char hdr[56];
    if (bytes >= 53) {
        size_t nread = bytes < sizeof(hdr) ? bytes : sizeof(hdr);
        if (fread(hdr, 1, nread, fp) != nread) {
            fclose(fp); Rf_error("could not read binary header");
        }

        size_t magic_len = 0;
        size_t candidate_header_size = 0;
        if (nread >= 16 && memcmp(hdr, hobbs_MAGIC_16, 16) == 0) {
            magic_len = 16;
            candidate_header_size = 56;
        } else if (nread >= 16 && memcmp(hdr, FASTAMH_MAGIC_16, 16) == 0) {
            magic_len = 16;
            candidate_header_size = 56;
        }

        if (candidate_header_size > 0) {
            if (bytes < candidate_header_size) {
                fclose(fp); Rf_error("file is shorter than hobbs binary header");
            }
            uint64_t version = read_u64_le(hdr + magic_len);
            uint64_t header_dim = read_u64_le(hdr + magic_len + 8);
            uint64_t header_rec_size = read_u64_le(hdr + magic_len + 32);
            if (version != 1) { fclose(fp); Rf_error("unsupported hobbs binary version"); }
            if (header_dim == 0 || header_dim > 100000000ULL) { fclose(fp); Rf_error("invalid dim in hobbs binary header"); }
            if (dim_requested > 0 && (uint64_t)dim_requested != header_dim) {
                fclose(fp); Rf_error("supplied dim does not match dim in binary header");
            }
            dim = (int)header_dim;
            rec_size = (size_t)header_rec_size;
            header_size = candidate_header_size;
        }
        rewind(fp);
    }

    if (header_size == 0) {
        if (dim <= 0) {
            fclose(fp);
            Rf_error("binary file has no hobbs header; please supply dim explicitly");
        }
        rec_size = (size_t)(24 + 8 * (size_t)dim);
    }

    size_t expected_rec_size = (size_t)(24 + 8 * (size_t)dim);
    if (rec_size != expected_rec_size) {
        fclose(fp);
        Rf_error("record size in binary header does not match dim");
    }
    if (bytes < header_size) { fclose(fp); Rf_error("file is shorter than binary header"); }
    size_t data_bytes = bytes - header_size;
    if (data_bytes % rec_size != 0) {
        fclose(fp);
        Rf_error("file size is not a multiple of expected record size after header");
    }
    size_t nrec = data_bytes / rec_size;
    if (max_records >= 0 && (size_t)max_records < nrec) nrec = (size_t)max_records;

    if (header_size > 0 && fseek(fp, (long)header_size, SEEK_SET) != 0) {
        fclose(fp); Rf_error("seek to first record failed");
    }

    SEXP ans = PROTECT(Rf_allocVector(VECSXP, 3 + dim));
    SEXP iter = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t)nrec));
    SEXP acc = PROTECT(Rf_allocVector(LGLSXP, (R_xlen_t)nrec));
    SEXP logp = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t)nrec));
    SET_VECTOR_ELT(ans, 0, iter);
    SET_VECTOR_ELT(ans, 1, acc);
    SET_VECTOR_ELT(ans, 2, logp);

    for (int j = 0; j < dim; ++j) {
        SEXP col = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t)nrec));
        SET_VECTOR_ELT(ans, 3 + j, col);
        UNPROTECT(1);
    }

    unsigned char* buf = (unsigned char*)malloc(rec_size);
    if (!buf) { fclose(fp); UNPROTECT(4); Rf_error("allocation failed"); }

    for (size_t i = 0; i < nrec; ++i) {
        if (fread(buf, 1, rec_size, fp) != rec_size) {
            free(buf); fclose(fp); UNPROTECT(4); Rf_error("short read");
        }
        REAL(iter)[i] = (double)read_u64_le(buf);
        LOGICAL(acc)[i] = buf[8] ? 1 : 0;
        REAL(logp)[i] = read_f64_le(buf + 16);
        for (int j = 0; j < dim; ++j) {
            SEXP col = VECTOR_ELT(ans, 3 + j);
            REAL(col)[i] = read_f64_le(buf + 24 + 8 * (size_t)j);
        }
    }

    free(buf);
    fclose(fp);
    UNPROTECT(4);
    return ans;
}

static const R_CallMethodDef CallEntries[] = {
    {"hobbs_read_bin", (DL_FUNC)&hobbs_read_bin, 3},
    {NULL, NULL, 0}
};

void R_init_hobbs(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
