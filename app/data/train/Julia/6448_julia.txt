
# begin enum cusparseStatus_t
typealias cusparseStatus_t Uint32
const CUSPARSE_STATUS_SUCCESS = (uint32)(0)
const CUSPARSE_STATUS_NOT_INITIALIZED = (uint32)(1)
const CUSPARSE_STATUS_ALLOC_FAILED = (uint32)(2)
const CUSPARSE_STATUS_INVALID_VALUE = (uint32)(3)
const CUSPARSE_STATUS_ARCH_MISMATCH = (uint32)(4)
const CUSPARSE_STATUS_MAPPING_ERROR = (uint32)(5)
const CUSPARSE_STATUS_EXECUTION_FAILED = (uint32)(6)
const CUSPARSE_STATUS_INTERNAL_ERROR = (uint32)(7)
const CUSPARSE_STATUS_MATRIX_TYPE_NOT_SUPPORTED = (uint32)(8)
const CUSPARSE_STATUS_ZERO_PIVOT = (uint32)(9)
# end enum cusparseStatus_t

typealias cusparseContext Void
typealias cusparseHandle_t Ptr{cusparseContext}
typealias cusparseMatDescr Void
typealias cusparseMatDescr_t Ptr{cusparseMatDescr}
typealias cusparseSolveAnalysisInfo Void
typealias cusparseSolveAnalysisInfo_t Ptr{cusparseSolveAnalysisInfo}
typealias csrsv2Info Void
typealias csrsv2Info_t Ptr{csrsv2Info}
typealias bsrsv2Info Void
typealias bsrsv2Info_t Ptr{bsrsv2Info}
typealias bsrsm2Info Void
typealias bsrsm2Info_t Ptr{bsrsm2Info}
typealias csric02Info Void
typealias csric02Info_t Ptr{csric02Info}
typealias bsric02Info Void
typealias bsric02Info_t Ptr{bsric02Info}
typealias csrilu02Info Void
typealias csrilu02Info_t Ptr{csrilu02Info}
typealias bsrilu02Info Void
typealias bsrilu02Info_t Ptr{bsrilu02Info}
typealias cusparseHybMat Void
typealias cusparseHybMat_t Ptr{cusparseHybMat}

# begin enum cusparsePointerMode_t
typealias cusparsePointerMode_t Uint32
const CUSPARSE_POINTER_MODE_HOST = (uint32)(0)
const CUSPARSE_POINTER_MODE_DEVICE = (uint32)(1)
# end enum cusparsePointerMode_t

# begin enum cusparseAction_t
typealias cusparseAction_t Uint32
const CUSPARSE_ACTION_SYMBOLIC = (uint32)(0)
const CUSPARSE_ACTION_NUMERIC = (uint32)(1)
# end enum cusparseAction_t

# begin enum cusparseMatrixType_t
typealias cusparseMatrixType_t Uint32
const CUSPARSE_MATRIX_TYPE_GENERAL = (uint32)(0)
const CUSPARSE_MATRIX_TYPE_SYMMETRIC = (uint32)(1)
const CUSPARSE_MATRIX_TYPE_HERMITIAN = (uint32)(2)
const CUSPARSE_MATRIX_TYPE_TRIANGULAR = (uint32)(3)
# end enum cusparseMatrixType_t

# begin enum cusparseFillMode_t
typealias cusparseFillMode_t Uint32
const CUSPARSE_FILL_MODE_LOWER = (uint32)(0)
const CUSPARSE_FILL_MODE_UPPER = (uint32)(1)
# end enum cusparseFillMode_t

# begin enum cusparseDiagType_t
typealias cusparseDiagType_t Uint32
const CUSPARSE_DIAG_TYPE_NON_UNIT = (uint32)(0)
const CUSPARSE_DIAG_TYPE_UNIT = (uint32)(1)
# end enum cusparseDiagType_t

# begin enum cusparseIndexBase_t
typealias cusparseIndexBase_t Uint32
const CUSPARSE_INDEX_BASE_ZERO = (uint32)(0)
const CUSPARSE_INDEX_BASE_ONE = (uint32)(1)
# end enum cusparseIndexBase_t

# begin enum cusparseOperation_t
typealias cusparseOperation_t Uint32
const CUSPARSE_OPERATION_NON_TRANSPOSE = (uint32)(0)
const CUSPARSE_OPERATION_TRANSPOSE = (uint32)(1)
const CUSPARSE_OPERATION_CONJUGATE_TRANSPOSE = (uint32)(2)
# end enum cusparseOperation_t

# begin enum cusparseDirection_t
typealias cusparseDirection_t Uint32
const CUSPARSE_DIRECTION_ROW = (uint32)(0)
const CUSPARSE_DIRECTION_COLUMN = (uint32)(1)
# end enum cusparseDirection_t

# begin enum cusparseHybPartition_t
typealias cusparseHybPartition_t Uint32
const CUSPARSE_HYB_PARTITION_AUTO = (uint32)(0)
const CUSPARSE_HYB_PARTITION_USER = (uint32)(1)
const CUSPARSE_HYB_PARTITION_MAX = (uint32)(2)
# end enum cusparseHybPartition_t

# begin enum cusparseSolvePolicy_t
typealias cusparseSolvePolicy_t Uint32
const CUSPARSE_SOLVE_POLICY_NO_LEVEL = (uint32)(0)
const CUSPARSE_SOLVE_POLICY_USE_LEVEL = (uint32)(1)
# end enum cusparseSolvePolicy_t
