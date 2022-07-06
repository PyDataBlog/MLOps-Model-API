# Julia wrapper for header: /Users/jgoldfar/.julia/v0.4/Sundials/deps/usr/include/kinsol/kinsol.h
# Automatically generated using Clang.jl wrap_c, version 0.0.0

type KINSOL_struct; end # dummy type to give us a typed pointer
typealias KINSOL_ptr Ptr{KINSOL_struct}

function KINCreate()
    ccall((:KINCreate,libsundials_kinsol),KINSOL_ptr,())
end

function KINSetErrHandlerFn(kinmem::KINSOL_ptr,ehfun::KINErrHandlerFn,eh_data::Ptr{Void})
    ccall((:KINSetErrHandlerFn,libsundials_kinsol),Cint,(KINSOL_ptr,KINErrHandlerFn,Ptr{Void}),kinmem,ehfun,eh_data)
end

function KINSetErrFile(kinmem::KINSOL_ptr,errfp::Ptr{Void})
    ccall((:KINSetErrFile,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Void}),kinmem,errfp)
end

function KINSetInfoHandlerFn(kinmem::KINSOL_ptr,ihfun::KINInfoHandlerFn,ih_data::Ptr{Void})
    ccall((:KINSetInfoHandlerFn,libsundials_kinsol),Cint,(KINSOL_ptr,KINInfoHandlerFn,Ptr{Void}),kinmem,ihfun,ih_data)
end

function KINSetInfoFile(kinmem::KINSOL_ptr,infofp::Ptr{Void})
    ccall((:KINSetInfoFile,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Void}),kinmem,infofp)
end

function KINSetUserData(kinmem::KINSOL_ptr,user_data::Ptr{Void})
    ccall((:KINSetUserData,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Void}),kinmem,user_data)
end

function KINSetPrintLevel(kinmemm::KINSOL_ptr,printfl::Int)
    ccall((:KINSetPrintLevel,libsundials_kinsol),Cint,(KINSOL_ptr,Cint),kinmemm,printfl)
end

function KINSetNumMaxIters(kinmem::KINSOL_ptr,mxiter::Int)
    ccall((:KINSetNumMaxIters,libsundials_kinsol),Cint,(KINSOL_ptr,Clong),kinmem,mxiter)
end

function KINSetNoInitSetup(kinmem::KINSOL_ptr,noInitSetup::Int)
    ccall((:KINSetNoInitSetup,libsundials_kinsol),Cint,(KINSOL_ptr,Cint),kinmem,noInitSetup)
end

function KINSetNoResMon(kinmem::KINSOL_ptr,noNNIResMon::Int)
    ccall((:KINSetNoResMon,libsundials_kinsol),Cint,(KINSOL_ptr,Cint),kinmem,noNNIResMon)
end

function KINSetMaxSetupCalls(kinmem::KINSOL_ptr,msbset::Int)
    ccall((:KINSetMaxSetupCalls,libsundials_kinsol),Cint,(KINSOL_ptr,Clong),kinmem,msbset)
end

function KINSetMaxSubSetupCalls(kinmem::KINSOL_ptr,msbsetsub::Int)
    ccall((:KINSetMaxSubSetupCalls,libsundials_kinsol),Cint,(KINSOL_ptr,Clong),kinmem,msbsetsub)
end

function KINSetEtaForm(kinmem::KINSOL_ptr,etachoice::Int)
    ccall((:KINSetEtaForm,libsundials_kinsol),Cint,(KINSOL_ptr,Cint),kinmem,etachoice)
end

function KINSetEtaConstValue(kinmem::KINSOL_ptr,eta::realtype)
    ccall((:KINSetEtaConstValue,libsundials_kinsol),Cint,(KINSOL_ptr,realtype),kinmem,eta)
end

function KINSetEtaParams(kinmem::KINSOL_ptr,egamma::realtype,ealpha::realtype)
    ccall((:KINSetEtaParams,libsundials_kinsol),Cint,(KINSOL_ptr,realtype,realtype),kinmem,egamma,ealpha)
end

function KINSetResMonParams(kinmem::KINSOL_ptr,omegamin::realtype,omegamax::realtype)
    ccall((:KINSetResMonParams,libsundials_kinsol),Cint,(KINSOL_ptr,realtype,realtype),kinmem,omegamin,omegamax)
end

function KINSetResMonConstValue(kinmem::KINSOL_ptr,omegaconst::realtype)
    ccall((:KINSetResMonConstValue,libsundials_kinsol),Cint,(KINSOL_ptr,realtype),kinmem,omegaconst)
end

function KINSetNoMinEps(kinmem::KINSOL_ptr,noMinEps::Int)
    ccall((:KINSetNoMinEps,libsundials_kinsol),Cint,(KINSOL_ptr,Cint),kinmem,noMinEps)
end

function KINSetMaxNewtonStep(kinmem::KINSOL_ptr,mxnewtstep::realtype)
    ccall((:KINSetMaxNewtonStep,libsundials_kinsol),Cint,(KINSOL_ptr,realtype),kinmem,mxnewtstep)
end

function KINSetMaxBetaFails(kinmem::KINSOL_ptr,mxnbcf::Int)
    ccall((:KINSetMaxBetaFails,libsundials_kinsol),Cint,(KINSOL_ptr,Clong),kinmem,mxnbcf)
end

function KINSetRelErrFunc(kinmem::KINSOL_ptr,relfunc::realtype)
    ccall((:KINSetRelErrFunc,libsundials_kinsol),Cint,(KINSOL_ptr,realtype),kinmem,relfunc)
end

function KINSetFuncNormTol(kinmem::KINSOL_ptr,fnormtol::realtype)
    ccall((:KINSetFuncNormTol,libsundials_kinsol),Cint,(KINSOL_ptr,realtype),kinmem,fnormtol)
end

function KINSetScaledStepTol(kinmem::KINSOL_ptr,scsteptol::realtype)
    ccall((:KINSetScaledStepTol,libsundials_kinsol),Cint,(KINSOL_ptr,realtype),kinmem,scsteptol)
end

function KINSetConstraints(kinmem::KINSOL_ptr,constraints::N_Vector)
    ccall((:KINSetConstraints,libsundials_kinsol),Cint,(KINSOL_ptr,N_Vector),kinmem,constraints)
end

function KINSetSysFunc(kinmem::KINSOL_ptr,func::KINSysFn)
    ccall((:KINSetSysFunc,libsundials_kinsol),Cint,(KINSOL_ptr,KINSysFn),kinmem,func)
end

function KINInit(kinmem::KINSOL_ptr,func::KINSysFn,tmpl::N_Vector)
    ccall((:KINInit,libsundials_kinsol),Cint,(KINSOL_ptr,KINSysFn,N_Vector),kinmem,func,tmpl)
end

function KINSol(kinmem::KINSOL_ptr,uu::N_Vector,strategy::Int,u_scale::N_Vector,f_scale::N_Vector)
    ccall((:KINSol,libsundials_kinsol),Cint,(KINSOL_ptr,N_Vector,Cint,N_Vector,N_Vector),kinmem,uu,strategy,u_scale,f_scale)
end

function KINGetWorkSpace(kinmem::KINSOL_ptr,lenrw::Ptr{Clong},leniw::Ptr{Clong})
    ccall((:KINGetWorkSpace,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Clong},Ptr{Clong}),kinmem,lenrw,leniw)
end

function KINGetNumNonlinSolvIters(kinmem::KINSOL_ptr,nniters::Ptr{Clong})
    ccall((:KINGetNumNonlinSolvIters,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Clong}),kinmem,nniters)
end

function KINGetNumFuncEvals(kinmem::KINSOL_ptr,nfevals::Ptr{Clong})
    ccall((:KINGetNumFuncEvals,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Clong}),kinmem,nfevals)
end

function KINGetNumBetaCondFails(kinmem::KINSOL_ptr,nbcfails::Ptr{Clong})
    ccall((:KINGetNumBetaCondFails,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Clong}),kinmem,nbcfails)
end

function KINGetNumBacktrackOps(kinmem::KINSOL_ptr,nbacktr::Ptr{Clong})
    ccall((:KINGetNumBacktrackOps,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Clong}),kinmem,nbacktr)
end

function KINGetFuncNorm(kinmem::KINSOL_ptr,fnorm::Ptr{realtype})
    ccall((:KINGetFuncNorm,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{realtype}),kinmem,fnorm)
end

function KINGetStepLength(kinmem::KINSOL_ptr,steplength::Ptr{realtype})
    ccall((:KINGetStepLength,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{realtype}),kinmem,steplength)
end

function KINGetReturnFlagName(flag::Int)
    ccall((:KINGetReturnFlagName,libsundials_kinsol),Ptr{UInt8},(Clong,),flag)
end

function KINFree(kinmem::Vector{KINSOL_ptr})
    ccall((:KINFree,libsundials_kinsol),Void,(Ptr{KINSOL_ptr},),kinmem)
end
# Julia wrapper for header: /Users/jgoldfar/.julia/v0.4/Sundials/deps/usr/include/kinsol/kinsol_direct.h
# Automatically generated using Clang.jl wrap_c, version 0.0.0

function KINDlsSetDenseJacFn(kinmem::KINSOL_ptr,jac::KINDlsDenseJacFn)
    ccall((:KINDlsSetDenseJacFn,libsundials_kinsol),Cint,(KINSOL_ptr,KINDlsDenseJacFn),kinmem,jac)
end

function KINDlsSetBandJacFn(kinmem::KINSOL_ptr,jac::KINDlsBandJacFn)
    ccall((:KINDlsSetBandJacFn,libsundials_kinsol),Cint,(KINSOL_ptr,KINDlsBandJacFn),kinmem,jac)
end

function KINDlsGetWorkSpace(kinmem::KINSOL_ptr,lenrwB::Ptr{Clong},leniwB::Ptr{Clong})
    ccall((:KINDlsGetWorkSpace,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Clong},Ptr{Clong}),kinmem,lenrwB,leniwB)
end

function KINDlsGetNumJacEvals(kinmem::KINSOL_ptr,njevalsB::Ptr{Clong})
    ccall((:KINDlsGetNumJacEvals,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Clong}),kinmem,njevalsB)
end

function KINDlsGetNumFuncEvals(kinmem::KINSOL_ptr,nfevalsB::Ptr{Clong})
    ccall((:KINDlsGetNumFuncEvals,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Clong}),kinmem,nfevalsB)
end

function KINDlsGetLastFlag(kinmem::KINSOL_ptr,flag::Ptr{Clong})
    ccall((:KINDlsGetLastFlag,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Clong}),kinmem,flag)
end

function KINDlsGetReturnFlagName(flag::Int)
    ccall((:KINDlsGetReturnFlagName,libsundials_kinsol),Ptr{UInt8},(Clong,),flag)
end
# Julia wrapper for header: /Users/jgoldfar/.julia/v0.4/Sundials/deps/usr/include/kinsol/kinsol_spils.h
# Automatically generated using Clang.jl wrap_c, version 0.0.0

function KINSpilsSetMaxRestarts(kinmem::KINSOL_ptr,maxrs::Int)
    ccall((:KINSpilsSetMaxRestarts,libsundials_kinsol),Cint,(KINSOL_ptr,Cint),kinmem,maxrs)
end

function KINSpilsSetPreconditioner(kinmem::KINSOL_ptr,pset::KINSpilsPrecSetupFn,psolve::KINSpilsPrecSolveFn)
    ccall((:KINSpilsSetPreconditioner,libsundials_kinsol),Cint,(KINSOL_ptr,KINSpilsPrecSetupFn,KINSpilsPrecSolveFn),kinmem,pset,psolve)
end

function KINSpilsSetJacTimesVecFn(kinmem::KINSOL_ptr,jtv::KINSpilsJacTimesVecFn)
    ccall((:KINSpilsSetJacTimesVecFn,libsundials_kinsol),Cint,(KINSOL_ptr,KINSpilsJacTimesVecFn),kinmem,jtv)
end

function KINSpilsGetWorkSpace(kinmem::KINSOL_ptr,lenrwSG::Ptr{Clong},leniwSG::Ptr{Clong})
    ccall((:KINSpilsGetWorkSpace,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Clong},Ptr{Clong}),kinmem,lenrwSG,leniwSG)
end

function KINSpilsGetNumPrecEvals(kinmem::KINSOL_ptr,npevals::Ptr{Clong})
    ccall((:KINSpilsGetNumPrecEvals,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Clong}),kinmem,npevals)
end

function KINSpilsGetNumPrecSolves(kinmem::KINSOL_ptr,npsolves::Ptr{Clong})
    ccall((:KINSpilsGetNumPrecSolves,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Clong}),kinmem,npsolves)
end

function KINSpilsGetNumLinIters(kinmem::KINSOL_ptr,nliters::Ptr{Clong})
    ccall((:KINSpilsGetNumLinIters,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Clong}),kinmem,nliters)
end

function KINSpilsGetNumConvFails(kinmem::KINSOL_ptr,nlcfails::Ptr{Clong})
    ccall((:KINSpilsGetNumConvFails,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Clong}),kinmem,nlcfails)
end

function KINSpilsGetNumJtimesEvals(kinmem::KINSOL_ptr,njvevals::Ptr{Clong})
    ccall((:KINSpilsGetNumJtimesEvals,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Clong}),kinmem,njvevals)
end

function KINSpilsGetNumFuncEvals(kinmem::KINSOL_ptr,nfevalsS::Ptr{Clong})
    ccall((:KINSpilsGetNumFuncEvals,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Clong}),kinmem,nfevalsS)
end

function KINSpilsGetLastFlag(kinmem::KINSOL_ptr,flag::Ptr{Clong})
    ccall((:KINSpilsGetLastFlag,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Clong}),kinmem,flag)
end

function KINSpilsGetReturnFlagName(flag::Int)
    ccall((:KINSpilsGetReturnFlagName,libsundials_kinsol),Ptr{UInt8},(Clong,),flag)
end
# Julia wrapper for header: /Users/jgoldfar/.julia/v0.4/Sundials/deps/usr/include/kinsol/kinsol_band.h
# Automatically generated using Clang.jl wrap_c, version 0.0.0

function KINBand(kinmem::KINSOL_ptr,N::Int,mupper::Int,mlower::Int)
    ccall((:KINBand,libsundials_kinsol),Cint,(KINSOL_ptr,Clong,Clong,Clong),kinmem,N,mupper,mlower)
end
# Julia wrapper for header: /Users/jgoldfar/.julia/v0.4/Sundials/deps/usr/include/kinsol/kinsol_bbdpre.h
# Automatically generated using Clang.jl wrap_c, version 0.0.0

function KINBBDPrecInit(kinmem::KINSOL_ptr,Nlocal::Int,mudq::Int,mldq::Int,mukeep::Int,mlkeep::Int,dq_rel_uu::realtype,gloc::KINLocalFn,gcomm::KINCommFn)
    ccall((:KINBBDPrecInit,libsundials_kinsol),Cint,(KINSOL_ptr,Clong,Clong,Clong,Clong,Clong,realtype,KINLocalFn,KINCommFn),kinmem,Nlocal,mudq,mldq,mukeep,mlkeep,dq_rel_uu,gloc,gcomm)
end

function KINBBDPrecGetWorkSpace(kinmem::KINSOL_ptr,lenrwBBDP::Ptr{Clong},leniwBBDP::Ptr{Clong})
    ccall((:KINBBDPrecGetWorkSpace,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Clong},Ptr{Clong}),kinmem,lenrwBBDP,leniwBBDP)
end

function KINBBDPrecGetNumGfnEvals(kinmem::KINSOL_ptr,ngevalsBBDP::Ptr{Clong})
    ccall((:KINBBDPrecGetNumGfnEvals,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Clong}),kinmem,ngevalsBBDP)
end
# Julia wrapper for header: /Users/jgoldfar/.julia/v0.4/Sundials/deps/usr/include/kinsol/kinsol_dense.h
# Automatically generated using Clang.jl wrap_c, version 0.0.0

function KINDense(kinmem::KINSOL_ptr,N::Int)
    ccall((:KINDense,libsundials_kinsol),Cint,(KINSOL_ptr,Clong),kinmem,N)
end
# Julia wrapper for header: /Users/jgoldfar/.julia/v0.4/Sundials/deps/usr/include/kinsol/kinsol_impl.h
# Automatically generated using Clang.jl wrap_c, version 0.0.0

function KINCreate()
    ccall((:KINCreate,libsundials_kinsol),KINSOL_ptr,())
end

function KINSetErrHandlerFn(kinmem::KINSOL_ptr,ehfun::KINErrHandlerFn,eh_data::Ptr{Void})
    ccall((:KINSetErrHandlerFn,libsundials_kinsol),Cint,(KINSOL_ptr,KINErrHandlerFn,Ptr{Void}),kinmem,ehfun,eh_data)
end

function KINSetErrFile(kinmem::KINSOL_ptr,errfp::Ptr{Void})
    ccall((:KINSetErrFile,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Void}),kinmem,errfp)
end

function KINSetInfoHandlerFn(kinmem::KINSOL_ptr,ihfun::KINInfoHandlerFn,ih_data::Ptr{Void})
    ccall((:KINSetInfoHandlerFn,libsundials_kinsol),Cint,(KINSOL_ptr,KINInfoHandlerFn,Ptr{Void}),kinmem,ihfun,ih_data)
end

function KINSetInfoFile(kinmem::KINSOL_ptr,infofp::Ptr{Void})
    ccall((:KINSetInfoFile,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Void}),kinmem,infofp)
end

function KINSetUserData(kinmem::KINSOL_ptr,user_data::Ptr{Void})
    ccall((:KINSetUserData,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Void}),kinmem,user_data)
end

function KINSetPrintLevel(kinmemm::KINSOL_ptr,printfl::Int)
    ccall((:KINSetPrintLevel,libsundials_kinsol),Cint,(KINSOL_ptr,Cint),kinmemm,printfl)
end

function KINSetNumMaxIters(kinmem::KINSOL_ptr,mxiter::Int)
    ccall((:KINSetNumMaxIters,libsundials_kinsol),Cint,(KINSOL_ptr,Clong),kinmem,mxiter)
end

function KINSetNoInitSetup(kinmem::KINSOL_ptr,noInitSetup::Int)
    ccall((:KINSetNoInitSetup,libsundials_kinsol),Cint,(KINSOL_ptr,Cint),kinmem,noInitSetup)
end

function KINSetNoResMon(kinmem::KINSOL_ptr,noNNIResMon::Int)
    ccall((:KINSetNoResMon,libsundials_kinsol),Cint,(KINSOL_ptr,Cint),kinmem,noNNIResMon)
end

function KINSetMaxSetupCalls(kinmem::KINSOL_ptr,msbset::Int)
    ccall((:KINSetMaxSetupCalls,libsundials_kinsol),Cint,(KINSOL_ptr,Clong),kinmem,msbset)
end

function KINSetMaxSubSetupCalls(kinmem::KINSOL_ptr,msbsetsub::Int)
    ccall((:KINSetMaxSubSetupCalls,libsundials_kinsol),Cint,(KINSOL_ptr,Clong),kinmem,msbsetsub)
end

function KINSetEtaForm(kinmem::KINSOL_ptr,etachoice::Int)
    ccall((:KINSetEtaForm,libsundials_kinsol),Cint,(KINSOL_ptr,Cint),kinmem,etachoice)
end

function KINSetEtaConstValue(kinmem::KINSOL_ptr,eta::realtype)
    ccall((:KINSetEtaConstValue,libsundials_kinsol),Cint,(KINSOL_ptr,realtype),kinmem,eta)
end

function KINSetEtaParams(kinmem::KINSOL_ptr,egamma::realtype,ealpha::realtype)
    ccall((:KINSetEtaParams,libsundials_kinsol),Cint,(KINSOL_ptr,realtype,realtype),kinmem,egamma,ealpha)
end

function KINSetResMonParams(kinmem::KINSOL_ptr,omegamin::realtype,omegamax::realtype)
    ccall((:KINSetResMonParams,libsundials_kinsol),Cint,(KINSOL_ptr,realtype,realtype),kinmem,omegamin,omegamax)
end

function KINSetResMonConstValue(kinmem::KINSOL_ptr,omegaconst::realtype)
    ccall((:KINSetResMonConstValue,libsundials_kinsol),Cint,(KINSOL_ptr,realtype),kinmem,omegaconst)
end

function KINSetNoMinEps(kinmem::KINSOL_ptr,noMinEps::Int)
    ccall((:KINSetNoMinEps,libsundials_kinsol),Cint,(KINSOL_ptr,Cint),kinmem,noMinEps)
end

function KINSetMaxNewtonStep(kinmem::KINSOL_ptr,mxnewtstep::realtype)
    ccall((:KINSetMaxNewtonStep,libsundials_kinsol),Cint,(KINSOL_ptr,realtype),kinmem,mxnewtstep)
end

function KINSetMaxBetaFails(kinmem::KINSOL_ptr,mxnbcf::Int)
    ccall((:KINSetMaxBetaFails,libsundials_kinsol),Cint,(KINSOL_ptr,Clong),kinmem,mxnbcf)
end

function KINSetRelErrFunc(kinmem::KINSOL_ptr,relfunc::realtype)
    ccall((:KINSetRelErrFunc,libsundials_kinsol),Cint,(KINSOL_ptr,realtype),kinmem,relfunc)
end

function KINSetFuncNormTol(kinmem::KINSOL_ptr,fnormtol::realtype)
    ccall((:KINSetFuncNormTol,libsundials_kinsol),Cint,(KINSOL_ptr,realtype),kinmem,fnormtol)
end

function KINSetScaledStepTol(kinmem::KINSOL_ptr,scsteptol::realtype)
    ccall((:KINSetScaledStepTol,libsundials_kinsol),Cint,(KINSOL_ptr,realtype),kinmem,scsteptol)
end

function KINSetConstraints(kinmem::KINSOL_ptr,constraints::N_Vector)
    ccall((:KINSetConstraints,libsundials_kinsol),Cint,(KINSOL_ptr,N_Vector),kinmem,constraints)
end

function KINSetSysFunc(kinmem::KINSOL_ptr,func::KINSysFn)
    ccall((:KINSetSysFunc,libsundials_kinsol),Cint,(KINSOL_ptr,KINSysFn),kinmem,func)
end

function KINInit(kinmem::KINSOL_ptr,func::KINSysFn,tmpl::N_Vector)
    ccall((:KINInit,libsundials_kinsol),Cint,(KINSOL_ptr,KINSysFn,N_Vector),kinmem,func,tmpl)
end

function KINSol(kinmem::KINSOL_ptr,uu::N_Vector,strategy::Int,u_scale::N_Vector,f_scale::N_Vector)
    ccall((:KINSol,libsundials_kinsol),Cint,(KINSOL_ptr,N_Vector,Cint,N_Vector,N_Vector),kinmem,uu,strategy,u_scale,f_scale)
end

function KINGetWorkSpace(kinmem::KINSOL_ptr,lenrw::Ptr{Clong},leniw::Ptr{Clong})
    ccall((:KINGetWorkSpace,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Clong},Ptr{Clong}),kinmem,lenrw,leniw)
end

function KINGetNumNonlinSolvIters(kinmem::KINSOL_ptr,nniters::Ptr{Clong})
    ccall((:KINGetNumNonlinSolvIters,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Clong}),kinmem,nniters)
end

function KINGetNumFuncEvals(kinmem::KINSOL_ptr,nfevals::Ptr{Clong})
    ccall((:KINGetNumFuncEvals,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Clong}),kinmem,nfevals)
end

function KINGetNumBetaCondFails(kinmem::KINSOL_ptr,nbcfails::Ptr{Clong})
    ccall((:KINGetNumBetaCondFails,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Clong}),kinmem,nbcfails)
end

function KINGetNumBacktrackOps(kinmem::KINSOL_ptr,nbacktr::Ptr{Clong})
    ccall((:KINGetNumBacktrackOps,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{Clong}),kinmem,nbacktr)
end

function KINGetFuncNorm(kinmem::KINSOL_ptr,fnorm::Ptr{realtype})
    ccall((:KINGetFuncNorm,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{realtype}),kinmem,fnorm)
end

function KINGetStepLength(kinmem::KINSOL_ptr,steplength::Ptr{realtype})
    ccall((:KINGetStepLength,libsundials_kinsol),Cint,(KINSOL_ptr,Ptr{realtype}),kinmem,steplength)
end

function KINGetReturnFlagName(flag::Int)
    ccall((:KINGetReturnFlagName,libsundials_kinsol),Ptr{UInt8},(Clong,),flag)
end

function KINFree(kinmem::Vector{KINSOL_ptr})
    ccall((:KINFree,libsundials_kinsol),Void,(Ptr{KINSOL_ptr},),kinmem)
end

function KINErrHandler(error_code::Int,_module::Ptr{UInt8},_function::Ptr{UInt8},msg::Ptr{UInt8},user_data::Ptr{Void})
    ccall((:KINErrHandler,libsundials_kinsol),Void,(Cint,Ptr{UInt8},Ptr{UInt8},Ptr{UInt8},Ptr{Void}),error_code,_module,_function,msg,user_data)
end

function KINInfoHandler(_module::Ptr{UInt8},_function::Ptr{UInt8},msg::Ptr{UInt8},user_data::Ptr{Void})
    ccall((:KINInfoHandler,libsundials_kinsol),Void,(Ptr{UInt8},Ptr{UInt8},Ptr{UInt8},Ptr{Void}),_module,_function,msg,user_data)
end
# Julia wrapper for header: /Users/jgoldfar/.julia/v0.4/Sundials/deps/usr/include/kinsol/kinsol_spbcgs.h
# Automatically generated using Clang.jl wrap_c, version 0.0.0

function KINSpbcg(kinmem::KINSOL_ptr,maxl::Int)
    ccall((:KINSpbcg,libsundials_kinsol),Cint,(KINSOL_ptr,Cint),kinmem,maxl)
end
# Julia wrapper for header: /Users/jgoldfar/.julia/v0.4/Sundials/deps/usr/include/kinsol/kinsol_spgmr.h
# Automatically generated using Clang.jl wrap_c, version 0.0.0

function KINSpgmr(kinmem::KINSOL_ptr,maxl::Int)
    ccall((:KINSpgmr,libsundials_kinsol),Cint,(KINSOL_ptr,Cint),kinmem,maxl)
end
# Julia wrapper for header: /Users/jgoldfar/.julia/v0.4/Sundials/deps/usr/include/kinsol/kinsol_sptfqmr.h
# Automatically generated using Clang.jl wrap_c, version 0.0.0

function KINSptfqmr(kinmem::KINSOL_ptr,maxl::Int)
    ccall((:KINSptfqmr,libsundials_kinsol),Cint,(KINSOL_ptr,Cint),kinmem,maxl)
end
# header: /usr/local/include/kinsol/kinsol_band.h
@ctypedef KINDlsDenseJacFn Ptr{:Void}
@ctypedef KINDlsBandJacFn Ptr{:Void}
@c Int32 KINDlsSetDenseJacFn (:KINSOL_ptr,:KINDlsDenseJacFn) shlib
@c Int32 KINDlsSetBandJacFn (:KINSOL_ptr,:KINDlsBandJacFn) shlib
@c Int32 KINDlsGetWorkSpace (:KINSOL_ptr,Ptr{:Clong},Ptr{:Clong}) shlib
@c Int32 KINDlsGetNumJacEvals (:KINSOL_ptr,Ptr{:Clong}) shlib
@c Int32 KINDlsGetNumFuncEvals (:KINSOL_ptr,Ptr{:Clong}) shlib
@c Int32 KINDlsGetLastFlag (:KINSOL_ptr,Ptr{:Clong}) shlib
@c Ptr{:Uint8} KINDlsGetReturnFlagName (:Clong,) shlib
@c Int32 KINBand (:KINSOL_ptr,:Clong,:Clong,:Clong) shlib

# header: /usr/local/include/kinsol/kinsol_bbdpre.h
@ctypedef KINCommFn Ptr{:Void}
@ctypedef KINLocalFn Ptr{:Void}
@c Int32 KINBBDPrecInit (:KINSOL_ptr,:Clong,:Clong,:Clong,:Clong,:Clong,:Realtype,:KINLocalFn,:KINCommFn) shlib
@c Int32 KINBBDPrecGetWorkSpace (:KINSOL_ptr,Ptr{:Clong},Ptr{:Clong}) shlib
@c Int32 KINBBDPrecGetNumGfnEvals (:KINSOL_ptr,Ptr{:Clong}) shlib

# header: /usr/local/include/kinsol/kinsol_dense.h
@c Int32 KINDense (:KINSOL_ptr,:Clong) shlib

# header: /usr/local/include/kinsol/kinsol_direct.h

# header: /usr/local/include/kinsol/kinsol.h
@ctypedef KINSysFn Ptr{:Void}
@ctypedef KINErrHandlerFn Ptr{:Void}
@ctypedef KINInfoHandlerFn Ptr{:Void}
@c KINSOL_ptr KINCreate () shlib
@c Int32 KINSetErrHandlerFn (:KINSOL_ptr,:KINErrHandlerFn,Ptr{:Void}) shlib
@c Int32 KINSetErrFile (:KINSOL_ptr,Ptr{:FILE}) shlib
@c Int32 KINSetInfoHandlerFn (:KINSOL_ptr,:KINInfoHandlerFn,Ptr{:Void}) shlib
@c Int32 KINSetInfoFile (:KINSOL_ptr,Ptr{:FILE}) shlib
@c Int32 KINSetUserData (:KINSOL_ptr,Ptr{:Void}) shlib
@c Int32 KINSetPrintLevel (:KINSOL_ptr,:Int32) shlib
@c Int32 KINSetNumMaxIters (:KINSOL_ptr,:Clong) shlib
@c Int32 KINSetNoInitSetup (:KINSOL_ptr,:Int32) shlib
@c Int32 KINSetNoResMon (:KINSOL_ptr,:Int32) shlib
@c Int32 KINSetMaxSetupCalls (:KINSOL_ptr,:Clong) shlib
@c Int32 KINSetMaxSubSetupCalls (:KINSOL_ptr,:Clong) shlib
@c Int32 KINSetEtaForm (:KINSOL_ptr,:Int32) shlib
@c Int32 KINSetEtaConstValue (:KINSOL_ptr,:realtype) shlib
@c Int32 KINSetEtaParams (:KINSOL_ptr,:realtype,:realtype) shlib
@c Int32 KINSetResMonParams (:KINSOL_ptr,:realtype,:realtype) shlib
@c Int32 KINSetResMonConstValue (:KINSOL_ptr,:realtype) shlib
@c Int32 KINSetNoMinEps (:KINSOL_ptr,:Int32) shlib
@c Int32 KINSetMaxNewtonStep (:KINSOL_ptr,:realtype) shlib
@c Int32 KINSetMaxBetaFails (:KINSOL_ptr,:Clong) shlib
@c Int32 KINSetRelErrFunc (:KINSOL_ptr,:realtype) shlib
@c Int32 KINSetFuncNormTol (:KINSOL_ptr,:realtype) shlib
@c Int32 KINSetScaledStepTol (:KINSOL_ptr,:realtype) shlib
@c Int32 KINSetConstraints (:KINSOL_ptr,:N_Vector) shlib
@c Int32 KINSetSysFunc (:KINSOL_ptr,:KINSysFn) shlib
@c Int32 KINInit (:KINSOL_ptr,:KINSysFn,:N_Vector) shlib
@c Int32 KINSol (:KINSOL_ptr,:N_Vector,:Int32,:N_Vector,:N_Vector) shlib
@c Int32 KINGetWorkSpace (:KINSOL_ptr,Ptr{:Clong},Ptr{:Clong}) shlib
@c Int32 KINGetNumNonlinSolvIters (:KINSOL_ptr,Ptr{:Clong}) shlib
@c Int32 KINGetNumFuncEvals (:KINSOL_ptr,Ptr{:Clong}) shlib
@c Int32 KINGetNumBetaCondFails (:KINSOL_ptr,Ptr{:Clong}) shlib
@c Int32 KINGetNumBacktrackOps (:KINSOL_ptr,Ptr{:Clong}) shlib
@c Int32 KINGetFuncNorm (:KINSOL_ptr,Ptr{:realtype}) shlib
@c Int32 KINGetStepLength (:KINSOL_ptr,Ptr{:realtype}) shlib
@c Ptr{:Uint8} KINGetReturnFlagName (:Clong,) shlib
@c Void KINFree (Ptr{:KINSOL_ptr},) shlib

# header: /usr/local/include/kinsol/kinsol_impl.h
# @ctypedef KINMem Ptr{:Void}
@c Void KINProcessError (:KINMem,:Int32,Ptr{:Uint8},Ptr{:Uint8},Ptr{:Uint8}) shlib
@c Void KINErrHandler (:Int32,Ptr{:Uint8},Ptr{:Uint8},Ptr{:Uint8},Ptr{:Void}) shlib
@c Void KINPrintInfo (:KINMem,:Int32,Ptr{:Uint8},Ptr{:Uint8},Ptr{:Uint8}) shlib
@c Void KINInfoHandler (Ptr{:Uint8},Ptr{:Uint8},Ptr{:Uint8},Ptr{:Void}) shlib

# header: /usr/local/include/kinsol/kinsol_spbcgs.h
@c Int32 KINSpbcg (:KINSOL_ptr,:Int32) shlib

# header: /usr/local/include/kinsol/kinsol_spgmr.h
@c Int32 KINSpgmr (:KINSOL_ptr,:Int32) shlib

# header: /usr/local/include/kinsol/kinsol_spils.h
@ctypedef KINSpilsPrecSetupFn Ptr{:Void}
@ctypedef KINSpilsPrecSolveFn Ptr{:Void}
@ctypedef KINSpilsJacTimesVecFn Ptr{:Void}
@c Int32 KINSpilsSetMaxRestarts (:KINSOL_ptr,:Int32) shlib
@c Int32 KINSpilsSetPreconditioner (:KINSOL_ptr,:KINSpilsPrecSetupFn,:KINSpilsPrecSolveFn) shlib
@c Int32 KINSpilsSetJacTimesVecFn (:KINSOL_ptr,:KINSpilsJacTimesVecFn) shlib
@c Int32 KINSpilsGetWorkSpace (:KINSOL_ptr,Ptr{:Clong},Ptr{:Clong}) shlib
@c Int32 KINSpilsGetNumPrecEvals (:KINSOL_ptr,Ptr{:Clong}) shlib
@c Int32 KINSpilsGetNumPrecSolves (:KINSOL_ptr,Ptr{:Clong}) shlib
@c Int32 KINSpilsGetNumLinIters (:KINSOL_ptr,Ptr{:Clong}) shlib
@c Int32 KINSpilsGetNumConvFails (:KINSOL_ptr,Ptr{:Clong}) shlib
@c Int32 KINSpilsGetNumJtimesEvals (:KINSOL_ptr,Ptr{:Clong}) shlib
@c Int32 KINSpilsGetNumFuncEvals (:KINSOL_ptr,Ptr{:Clong}) shlib
@c Int32 KINSpilsGetLastFlag (:KINSOL_ptr,Ptr{:Clong}) shlib
@c Ptr{:Uint8} KINSpilsGetReturnFlagName (:Clong,) shlib

# header: /usr/local/include/kinsol/kinsol_sptfqmr.h
@c Int32 KINSptfqmr (:KINSOL_ptr,:Int32) shlib
