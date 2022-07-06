

PORTNAME=	openpegasus
PORTVERSION=	2.14.0
CATEGORIES=	net


MAINTAINER=	brunolauze@msn.com
COMMENT=	OpenPegasus Web Management Instrumentation

USES=		gettext gmake pkgconfig perl5
GNU_CONFIGURE=	yes

#USE_GITHUB=	yes
#GH_ACCOUNT=	brunolauze
#GH_PROJECT=	pegasus
#GH_COMMIT=	9673477
#GH_TAGNAME=	${GH_COMMIT}

LIB_DEPENDS=	libslp.so:${PORTSDIR}/net/openslp

PEGASUS_ROOT=${WRKSRC}
PEGASUS_HOME=${PEGASUS_ROOT}/out/Debug
PEGASUS_PLATFORM=FREEBSD_X86_64_CLANG

#PEGASUS_PREFIX=${STAGEDIR}${PREFIX}
PEGASUS_PREFIX=${PREFIX}

# OpenPegasus SSL support
OPENSSL_HOME=/usr
PEGASUS_HAS_SSL=yes
PEGASUS_TRACE_FILE_PATH=/var/log/cimserver.log
PEGASUS_LOG_DIR=/var/log/pegasus

PEGASUS_CONFIG_DIR="${PREFIX}/etc/pegasus"
PEGASUS_SSL_CERT_FILE_PATH="${PREFIX}/etc/pegasus/server.pem"
PEGASUS_SSL_KEY_FILE_PATH="${PREFIX}/etc/pegasus/file.pem"
PEGASUS_SSL_SERVER_TRUSTSTORE="${PREFIX}/etc/pegasus"
PEGASUS_CRL_VERIFICATION="${PREFIX}/etc/pegasus"
PEGASUS_SSL_SERVER_CRL="${PREFIX}/etc/pegasus"
PEGASUS_REPOSITORY_DIR="${PREFIX}/share/pegasus/repository"

# OpenPegasus SLP support
PEGASUS_OPENSLP_HOME=${LOCALBASE}
PEGASUS_USE_OPENSLP=true
PEGASUS_ENABLE_SLP=true
PEGASUS_USE_EXTERNAL_SLP=true

# OpenPegasus User authentication & authorization
PEGASUS_ENABLE_USERGROUP_AUTHORIZATION=true
PEGASUS_PAM_AUTHENTICATION=true

# OpenPegasus ExecQuery() method support
PEGASUS_ENABLE_EXECQUERY=true

PEGASUS_ENABLE_CMPI_PROVIDER_MANAGER=true
PEGASUS_SUPPORTS_DYNLIB="yes"
PEGASUS_USE_EXPERIMENTAL_INTERFACES=true
PEGASUS_DISABLE_DEPRECATED_INTERFACES=false
PEGASUS_REPOSITORY_MODE=BIN
PEGASUS_ADVANCED_CIMCLIENT=true
PEGASUS_ENABLE_PROTOCOL_BINARY=true
PEGASUS_ENABLE_PROTOCOL_WSMAN=true

CFLAGS+=        -DPEGASUS_PAM_AUTHENTICATION -DPEGASUS_ENABLE_USERGROUP_AUTHORIZATION \
                -DPEGASUS_ENABLE_EXECQUERY -DPEGASUS_ENABLE_SLP -DPEGASUS_HAS_SSL \
                -DPEGASUS_USE_EXTERNAL_SLP

CXXFLAGS+=      -DOPENSSL_HOME=${OPENSSL_HOME} -DPEGASUS_HAS_SSL=1 \
                -DPEGASUS_OPENSLP_HOME=\"${PEGASUS_OPENSLP_HOME}\" \
                -DPEGASUS_USE_OPENSLP=1 \
                -DPEGASUS_ENABLE_SLP=1 \
		-DPEGASUS_USE_EXTERNAL_SLP=1 \
                -DPEGASUS_ENABLE_USERGROUP_AUTHORIZATION=1 \
                -DPEGASUS_PAM_AUTHENTICATION=1 \
                -DPEGASUS_ENABLE_EXECQUERY=1 \
                -DPEGASUS_ENABLE_CMPI_PROVIDER_MANAGER=1 \
                -DPEGASUS_SUPPORTS_DYNLIB=1 \
                -DPEGASUS_USE_EXPERIMENTAL_INTERFACES=1 \
                -DPEGASUS_DISABLE_DEPRECATED_INTERFACES=1 \
                -DPEGASUS_REPOSITORY_MODE=BIN \
                -DPEGASUS_ADVANCED_CIMCLIENT=1 \
                -DPEGASUS_ENABLE_PROTOCOL_BINARY=1 \
		-DPEGASUS_ENABLE_PROTOCOL_WSMAN \
		-DPEGASUS_CONFIG_DIR=\"${PEGASUS_CONFIG_DIR}\" \
		-DPEGASUS_SSL_CERT_FILE_PATH=\"${PEGASUS_SSL_CERT_FILE_PATH}\" \
		-DPEGASUS_SSL_KEY_FILE_PATH=\"${PEGASUS_SSL_KEY_FILE_PATH}\" \
		-DPEGASUS_SSL_SERVER_TRUSTSTORE=\"${PEGASUS_SSL_SERVER_TRUSTSTORE}\" \
		-DPEGASUS_CRL_VERIFICATION=\"${PEGASUS_CRL_VERIFICATION}\" \
		-DPEGASUS_SSL_SERVER_CRL=\"${PEGASUS_SSL_SERVER_CRL}\" \
		-DPEGASUS_REPOSITORY_DIR=\"${PEGASUS_REPOSITORY_DIR}\"


CONFIGURE_ENV+= PEGASUS_ROOT=${PEGASUS_ROOT} PEGASUS_HOME=${PEGASUS_HOME} \
                PEGASUS_PLATFORM=${PEGASUS_PLATFORM} OPENSSL_HOME=${OPENSSL_HOME} \
                PEGASUS_HAS_SSL=${PEGASUS_HAS_SSL} \
                PEGASUS_OPENSLP_HOME=${PEGASUS_OPENSLP_HOME} \
                PEGASUS_USE_OPENSLP=${PEGASUS_USE_OPENSLP} \
                PEGASUS_ENABLE_SLP=${PEGASUS_ENABLE_SLP} \
                PEGASUS_ENABLE_USERGROUP_AUTHORIZATION=${PEGASUS_ENABLE_USERGROUP_AUTHORIZATION} \
                PEGASUS_PAM_AUTHENTICATION=${PEGASUS_PAM_AUTHENTICATION} \
                PEGASUS_ENABLE_EXECQUERY=${PEGASUS_ENABLE_EXECQUERY} \
		PEGASUS_ENABLE_PROTOCOL_WSMAN=${PEGASUS_ENABLE_PROTOCOL_WSMAN}

MAKE_ENV+=      PEGASUS_ROOT=${PEGASUS_ROOT} PEGASUS_HOME=${PEGASUS_HOME} \
                PEGASUS_PLATFORM=${PEGASUS_PLATFORM} OPENSSL_HOME=${OPENSSL_HOME} \
                PEGASUS_HAS_SSL=${PEGASUS_HAS_SSL} \
                PEGASUS_OPENSLP_HOME=${PEGASUS_OPENSLP_HOME} \
                PEGASUS_USE_OPENSLP=${PEGASUS_USE_OPENSLP} \
                PEGASUS_ENABLE_SLP=${PEGASUS_ENABLE_SLP} \
                PEGASUS_ENABLE_USERGROUP_AUTHORIZATION=${PEGASUS_ENABLE_USERGROUP_AUTHORIZATION} \
                PEGASUS_PAM_AUTHENTICATION=${PEGASUS_PAM_AUTHENTICATION} \
                PEGASUS_ENABLE_EXECQUERY=${PEGASUS_ENABLE_EXECQUERY} \
                PEGASUS_ENABLE_CMPI_PROVIDER_MANAGER=${PEGASUS_ENABLE_CMPI_PROVIDER_MANAGER} \
                PEGASUS_SUPPORTS_DYNLIB=${PEGASUS_SUPPORTS_DYNLIB} \
                PEGASUS_USE_EXPERIMENTAL_INTERFACES=${PEGASUS_USE_EXPERIMENTAL_INTERFACES} \
                PEGASUS_DISABLE_DEPRECATED_INTERFACES=${PEGASUS_DISABLE_DEPRECATED_INTERFACES} \
                PEGASUS_REPOSITORY_MODE=${PEGASUS_REPOSITORY_MODE} \
                PEGASUS_ADVANCED_CIMCLIENT=${PEGASUS_ADVANCED_CIMCLIENT} \
                PEGASUS_ENABLE_PROTOCOL_BINARY=${PEGASUS_ENABLE_PROTOCOL_BINARY} \
		PEGASUS_ENABLE_PROTOCOL_WSMAN=${PEGASUS_ENABLE_PROTOCOL_WSMAN} \
		PEGASUS_CONFIG_DIR=${PEGASUS_CONFIG_DIR} \
		PEGASUS_SSL_CERT_FILE_PATH=${PEGASUS_SSL_CERT_FILE_PATH} \
		PEGASUS_SSL_KEY_FILE_PATH=${PEGASUS_SSL_KEY_FILE_PATH} \
		PEGASUS_SSL_SERVER_TRUSTSTORE=${PEGASUS_SSL_SERVER_TRUSTSTORE} \
		PEGASUS_CRL_VERIFICATION=${PEGASUS_CRL_VERIFICATION} \
		PEGASUS_SSL_SERVER_CRL=${PEGASUS_SSL_SERVER_CRL} \
		PEGASUS_REPOSITORY_DIR=${PEGASUS_REPOSITORY_DIR} \
		CFLAGS="${CFLAGS}" CXXFLAGS="${CXXFLAGS}" DEFINES="${CXXFLAGS}" \
                LD_LIBRARY_PATH=${PEGASUS_HOME}/lib: \
                PATH=${PEGASUS_HOME}/bin:${LOCALBASE}/bin:${LOCALBASE}/sbin:/sbin:/usr/bin:/bin


SUB_FILES=	SampleCMPIInstanceClient \
		TestAbstract \
		TestAggregationOutputClient \
		TestAnonymousPipe \
		TestAnonymousPipeChild \
		TestArray \
		TestAssocClassCache \
		TestAssocOperations \
		TestAssocTable \
		TestAssociationTestClient \
		TestAsyncCallback \
		TestAsyncQueue \
		TestAtomicInt \
		TestAttribute \
		TestAuditLogger \
		TestAuthenticationInfo \
		TestAuthenticationManager \
		TestAutoPtr \
		TestBase64 \
		TestBasicAuthenticationHandler \
		TestBenchmark \
		TestBinaryClient \
		TestBuffer \
		TestCIMBuffer \
		TestCIMErrorClient \
		TestCIMName \
		TestCIMOMHandleTestProviderClient \
		TestCIMOMStatData \
		TestCIMQueryCap \
		TestCIMxmlIndicationHandlerDestination \
		TestCMPIAssociation \
		TestCMPIBroker \
		TestCMPIBrokerEnc \
		TestCMPIEnumeration \
		TestCMPIError \
		TestCMPIIndicationSubscription \
		TestCMPIInstance \
		TestCMPIInstanceExecQuery \
		TestCMPIKeyReturned \
		TestCMPIMethod \
		TestCMPIProperty \
		TestCMPIThreadProvider \
		TestCMPI_Fail_1 \
		TestCMPI_Fail_2 \
		TestCMPI_Fail_3 \
		TestCMPI_Fail_5 \
		TestCQL \
		TestCQLChainedIdentifier \
		TestCQLRegularExpression \
		TestCQLValue \
		TestCertClient \
		TestChgdHosts \
		TestChunkingStressProviderClient \
		TestCimError \
		TestCimsub \
		TestCimsubInterOp \
		TestClassDecl \
		TestClassDeclRep \
		TestClient \
		TestCompAssoc \
		TestCompare \
		TestCompareRepositories \
		TestConfig \
		TestConfigFileHandler \
		TestConfigManager \
		TestDateTime \
		TestDeliveryRetry \
		TestDir \
		TestDisableEnable \
		TestDisableEnable2 \
		TestDynListener \
		TestDynamicLibrary \
		TestEmbeddedInstance \
		TestEnumerateClassNames \
		TestExecutor \
		TestExportClient \
		TestFaultyInstanceProviderClient \
		TestFileHandler \
		TestFileLock \
		TestFileSystem \
		TestFlavor \
		TestFormatter \
		TestG11n \
		TestGroupMethodProvider \
		TestHTTPExportResponseDecoder \
		TestHTTPMessage \
		TestHandlerTable \
		TestHashTable \
		TestHostAddress \
		TestHostLocator \
		TestIDFactory \
		TestIdleConnectionTimeout \
		TestIndicationProcess \
		TestIndicationService \
		TestIndicationStressTest \
		TestIndicationsCount \
		TestInheritanceTree \
		TestInst \
		TestInstAlias \
		TestInstanceDecl \
		TestInstanceProviderClient \
		TestInterop \
		TestL10N \
		TestList \
		TestListenAddress \
		TestListener \
		TestLocalAuthFile \
		TestLocalAuthenticationHandler \
		TestLogger \
		TestMCCATestClient \
		TestMessageQueue \
		TestMessageQueueService \
		TestMessageSerializer \
		TestMethod \
		TestMethodTestProvider \
		TestModelWalkStressClient \
		TestModuleController \
		TestMutex \
		TestNameSpace \
		TestNameSpaceManager \
		TestOOPModuleFailure \
		TestObject \
		TestObjectNormalizer \
		TestOperationContext \
		TestOperationResponseHandler \
		TestOptionManager \
		TestOrderedSet \
		TestParamValue \
		TestParameter \
		TestParser \
		TestParserCLI \
		TestPegClientAssociationClient \
		TestPegClientClient \
		TestPegClientClientStatistics \
		TestPegClientDeleteNamespace \
		TestPegClientEnumInstances \
		TestPegClientInvokeMethod2 \
		TestPegClientSLPClient \
		TestPegClientSSLCipherVerification \
		TestProcessIndication \
		TestProperty \
		TestProvidMgrModifyInstances \
		TestProvidMgrPluginMap \
		TestProviderLifecycleIndications \
		TestProviderManager \
		TestProviderMessageHandler \
		TestProviderModule \
		TestProviderName \
		TestProviderStatus \
		TestPrvRegMgrCreateInstances \
		TestPrvRegMgrDeleteInstances \
		TestPrvRegMgrLookupIndicationConsumer \
		TestPrvRegMgrLookupIndicationProvider \
		TestPrvRegMgrLookupInstanceProvider \
		TestPrvRegMgrLookupMethodProvider \
		TestPrvRegMgrWildCardNamespaceNames \
		TestQualifier \
		TestQualifierDecl \
		TestQualifierDeclCompile \
		TestQualifierDeclRep \
		TestQualifierList \
		TestQueryContext \
		TestQueryExpression \
		TestRT_Sample_Subscription \
		TestReference \
		TestRepository \
		TestRepository2 \
		TestRepositoryMix \
		TestResolve \
		TestResponseHandler \
		TestSCMO \
		TestSLP \
		TestSLP_SA \
		TestScope \
		TestServerProfile \
		TestSharedInheritanceTree \
		TestSharedNameSpace \
		TestSharedPtr \
		TestSpinLock \
		TestStack \
		TestStaticClient \
		TestStatisticalData \
		TestStopwatch \
		TestStrToInstName \
		TestStressTestController \
		TestString \
		TestStringConversion \
		TestSubscription \
		TestSubscriptionKey \
		TestSystem \
		TestThread \
		TestThreadPool \
		TestTimeValue \
		TestToMof \
		TestTracer \
		TestUintArgs \
		TestUpgrade \
		TestUserContext \
		TestValidateClass \
		TestValue \
		TestWrapperStressClient \
		TestXmlDump \
		TestXmlParser \
		TestXmlPrint \
		TestXmlReader \
		TestXmlWriter \
		TestcreateRegistration \
		TestmodifyRegistration \
		chksrc \
		cimauth \
		cimcli \
		cimconfig \
		cimcrl \
		cimlistener \
		cimmof \
		cimmofl \
		cimperf \
		cimprovagt \
		cimprovider \
		cimreparchive \
		cimserver \
		cimserverd \
		cimsub \
		cimtrust \
		cnv2rootbundle \
		insertcrs \
		ipinfo \
		loadxml \
		osinfo \
		repupgrade \
		slp_attrreq \
		slp_query \
		slp_srvreg \
		srv_reg \
		stripcrs \
		stripline \
		testCQLExecQuery \
		testExecQuery \
		tomof \
		wbemexec \
		cimserver_planned.conf \

SUB_LIST+=	COMMENT="${COMMENT}" \
		PEGASUS_PREFIX=${PEGASUS_PREFIX}

cimquery:
	@(cd ${WRKSRC}/src/Pegasus/WQL/tests/ExecQueryClient && ${MAKE_ENV} gmake clean && ${MAKE_ENV} gmake)
	@(cp ${PEGASUS_HOME}/bin/cimquery ${STAGEDIR}${PREFIX}/lib/pegasus/bin/cimquery)

.include <bsd.port.pre.mk>

configure: do-configure

do-configure: patch
	@(echo "Configuration done...")

post-build: repository

repository:
	@(cd ${WRKSRC} && ${MAKE_ENV} ${GMAKE} repository)
#	@(cd ${WRKSRC} && ${MAKE_ENV} ${GMAKE} repositoryServer)
	@(cd ${WRKSRC} && ${MAKE_ENV} ${GMAKE} setupdevserver)

do-install:
	${MKDIR} ${STAGEDIR}${PREFIX}/lib/pegasus
	${MKDIR} ${STAGEDIR}${PREFIX}/lib/pegasus/bin
	${MKDIR} ${STAGEDIR}${PREFIX}/lib/pegasus/lib
	${MKDIR} ${STAGEDIR}${PREFIX}/etc/pegasus
	${MKDIR} ${STAGEDIR}${PREFIX}/www
	${MKDIR} ${STAGEDIR}${PREFIX}/www/pegasus
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libAlertIndicationProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libAlertIndicationProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libCIMOMHandleTestProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCIMOMHandleTestProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libCIMOMStatDataProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCIMOMStatDataProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libCIMQueryCapabilitiesProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCIMQueryCapabilitiesProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libCIMxmlIndicationHandler.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCIMxmlIndicationHandler.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libCLITestProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCLITestProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libCMPIProviderManager.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCMPIProviderManager.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libCMPISampleClass.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCMPISampleClass.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libCMPI_RT_Sample.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCMPI_RT_Sample.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libCertificateProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCertificateProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libConfigSettingProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libConfigSettingProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libDefaultProviderManager.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libDefaultProviderManager.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libDeliveryRetryTestProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libDeliveryRetryTestProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libDynamicIndicationProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libDynamicIndicationProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libEmbeddedInstanceProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libEmbeddedInstanceProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libFamilyProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libFamilyProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libFileListenerDestination.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libFileListenerDestination.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libFilesAndDirectories.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libFilesAndDirectories.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libFilesAndDirectories_CXX.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libFilesAndDirectories_CXX.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libIndicationStressTestConsumer.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libIndicationStressTestConsumer.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libIndicationStressTestProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libIndicationStressTestProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libIndicationTestProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libIndicationTestProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libInteropProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libInteropProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libMCCATestAssocProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libMCCATestAssocProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libMethodTestProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libMethodTestProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libMissingEntryPointHandler.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libMissingEntryPointHandler.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libNamespaceProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libNamespaceProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libOOPModuleFailureTestProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libOOPModuleFailureTestProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libPG_DefaultInstanceProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libPG_DefaultInstanceProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libPG_TestPropertyTypes.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libPG_TestPropertyTypes.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libProcessIndicationProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libProcessIndicationProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libProviderLifecycleIndicationConsumer.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libProviderLifecycleIndicationConsumer.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libProviderRegistrationProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libProviderRegistrationProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libSLPProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libSLPProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libSampleAssociationProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libSampleAssociationProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libSampleIndicationProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libSampleIndicationProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libSampleInstanceProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libSampleInstanceProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libSampleInstanceQueryProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libSampleInstanceQueryProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libSampleMethodProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libSampleMethodProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libSimpleDisplayConsumer.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libSimpleDisplayConsumer.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libTestAggregationOutputProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestAggregationOutputProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libTestCIM_ErrorProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestCIM_ErrorProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libTestCMPIAssociationProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestCMPIAssociationProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libTestCMPIInstanceProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestCMPIInstanceProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libTestCMPI_CXX.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestCMPI_CXX.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libTestChunkingStressProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestChunkingStressProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libTestDynLib.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestDynLib.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libTestFaultyProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestFaultyProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libTestGoodInstanceProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestGoodInstanceProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libTestGroupingProvider1.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestGroupingProvider1.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libTestGroupingProvider2.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestGroupingProvider2.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libTestInstanceProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestInstanceProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libTestLocalizedProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestLocalizedProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libTestProviderLifecycleIndicationProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestProviderLifecycleIndicationProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libTestServerProfileProviders.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestServerProfileProviders.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libTestStressTestClient.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestStressTestClient.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUserContextTestProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUserContextTestProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libbenchmarkDefinition.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libbenchmarkDefinition.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libbenchmarkProviderModule.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libbenchmarkProviderModule.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libcmpiCWS_Util.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiCWS_Util.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libcmpiCppImpl.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiCppImpl.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libcmpiIndicationStressTestProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiIndicationStressTestProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libcmpiPerf_TestAssocClassProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiPerf_TestAssocClassProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libcmpiPerf_TestClassAProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiPerf_TestClassAProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libcmpiPerf_TestClassProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiPerf_TestClassProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libcmpiSampleInstanceProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiSampleInstanceProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libcmpiSampleMethodProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiSampleMethodProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libcmpiTestBrokerEncProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestBrokerEncProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libcmpiTestBrokerProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestBrokerProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libcmpiTestCMPIThreadProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestCMPIThreadProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libcmpiTestEnumerationProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestEnumerationProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libcmpiTestErrorProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestErrorProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libcmpiTestExecQueryProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestExecQueryProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libcmpiTestFail_1Provider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestFail_1Provider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libcmpiTestFail_2Provider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestFail_2Provider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libcmpiTestFail_3Provider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestFail_3Provider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libcmpiTestFail_4Provider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestFail_4Provider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libcmpiTestFail_5Provider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestFail_5Provider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libcmpiTestIndicationProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestIndicationProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libcmpiTestKeyReturnedProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestKeyReturnedProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libcmpiTestMethodProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestMethodProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libcmpiUtilLib.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiUtilLib.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libgroupUtilLib.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libgroupUtilLib.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegauthentication.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegauthentication.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegclient.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegclient.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegcliutils.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegcliutils.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegcommon.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegcommon.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegcompiler.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegcompiler.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegconfig.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegconfig.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegcql.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegcql.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegdynlistener.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegdynlistener.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegexportclient.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegexportclient.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegexportserver.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegexportserver.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpeggeneral.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpeggeneral.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpeggetoopt.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpeggetoopt.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpeghandler.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpeghandler.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpeghandlerservice.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpeghandlerservice.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegindicationservice.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegindicationservice.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpeglistener.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpeglistener.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegmanagedclient.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegmanagedclient.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegpmrouter.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegpmrouter.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegpmservice.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegpmservice.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegprm.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegprm.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegprovider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegprovider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegprovidermanager.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegprovidermanager.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegquerycommon.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegquerycommon.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegqueryexpression.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegqueryexpression.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegrepository.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegrepository.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegsamplesspmodule.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegsamplesspmodule.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegserver.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegserver.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegservice.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegservice.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegslp.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegslp.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegslp_client.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegslp_client.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegwql.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegwql.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegwsmserver.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegwsmserver.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libpegwebserver.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegwebserver.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libcimrsserver.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcimrsserver.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libMeta_ClassProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libMeta_ClassProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_AccountProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_AccountProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_AccountManagementCapabilitiesProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_AccountManagementCapabilitiesProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_AccountOnSystemProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_AccountOnSystemProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_ApplicationSystemProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_ApplicationSystemProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_BIOSElementProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_BIOSElementProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_BIOSFeatureProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_BIOSFeatureProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_BIOSStringProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_BIOSStringProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_ClusterProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_ClusterProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_CurrentTimeProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_CurrentTimeProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_ComputerSystemProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_ComputerSystemProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_ComputerSystemPackageProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_ComputerSystemPackageProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_CreateDirectoryActionProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_CreateDirectoryActionProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_DatabaseServiceProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_DatabaseServiceProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_DatabaseSystemProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_DatabaseSystemProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_DeviceFileProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_DeviceFileProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_DirectoryProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_DirectoryProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_DiskDriveProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_DiskDriveProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_DiskPartitionProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_DiskPartitionProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_EthernetAdapterProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_EthernetAdapterProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_EthernetPortProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_EthernetPortProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_FileSystemProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_FileSystemProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_GroupProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_GroupProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_InstalledOSProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_InstalledOSProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_InstalledSoftwareElementProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_InstalledSoftwareElementProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_LocalFileSystemProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_LocalFileSystemProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_LogEntryProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_LogEntryProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_LogicalDiskProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_LogicalDiskProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_MessageLogProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_MessageLogProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_MountProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_MountProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_NFSProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_NFSProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_OSProcessProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_OSProcessProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_OperatingSystemProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_OperatingSystemProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_ProcessProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_ProcessProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_ProcessorProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_ProcessorProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_RemoteFileSystemProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_RemoteFileSystemProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_SoftwareElementProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_SoftwareElementProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_SoftwareFeatureProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_SoftwareFeatureProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_SystemBIOSProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_SystemBIOSProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_UsersAccessProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_UsersAccessProvider.so.1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/lib/libUNIX_VirtualComputerSystemProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_VirtualComputerSystemProvider.so.1
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libAlertIndicationProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libAlertIndicationProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCIMOMHandleTestProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCIMOMHandleTestProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCIMOMStatDataProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCIMOMStatDataProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCIMQueryCapabilitiesProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCIMQueryCapabilitiesProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCIMxmlIndicationHandler.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCIMxmlIndicationHandler.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCLITestProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCLITestProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCMPIProviderManager.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCMPIProviderManager.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCMPISampleClass.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCMPISampleClass.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCMPI_RT_Sample.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCMPI_RT_Sample.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCertificateProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libCertificateProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libConfigSettingProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libConfigSettingProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libDefaultProviderManager.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libDefaultProviderManager.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libDeliveryRetryTestProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libDeliveryRetryTestProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libDynamicIndicationProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libDynamicIndicationProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libEmbeddedInstanceProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libEmbeddedInstanceProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libFamilyProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libFamilyProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libFileListenerDestination.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libFileListenerDestination.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libFilesAndDirectories.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libFilesAndDirectories.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libFilesAndDirectories_CXX.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libFilesAndDirectories_CXX.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libIndicationStressTestConsumer.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libIndicationStressTestConsumer.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libIndicationStressTestProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libIndicationStressTestProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libIndicationTestProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libIndicationTestProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libInteropProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libInteropProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libMCCATestAssocProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libMCCATestAssocProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libMethodTestProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libMethodTestProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libMissingEntryPointHandler.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libMissingEntryPointHandler.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libNamespaceProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libNamespaceProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libOOPModuleFailureTestProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libOOPModuleFailureTestProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libPG_DefaultInstanceProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libPG_DefaultInstanceProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libPG_TestPropertyTypes.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libPG_TestPropertyTypes.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libProcessIndicationProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libProcessIndicationProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libProviderLifecycleIndicationConsumer.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libProviderLifecycleIndicationConsumer.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libProviderRegistrationProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libProviderRegistrationProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libSLPProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libSLPProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libSampleAssociationProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libSampleAssociationProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libSampleIndicationProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libSampleIndicationProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libSampleInstanceProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libSampleInstanceProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libSampleInstanceQueryProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libSampleInstanceQueryProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libSampleMethodProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libSampleMethodProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libSimpleDisplayConsumer.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libSimpleDisplayConsumer.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestAggregationOutputProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestAggregationOutputProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestCIM_ErrorProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestCIM_ErrorProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestCMPIAssociationProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestCMPIAssociationProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestCMPIInstanceProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestCMPIInstanceProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestCMPI_CXX.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestCMPI_CXX.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestChunkingStressProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestChunkingStressProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestDynLib.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestDynLib.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestFaultyProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestFaultyProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestGoodInstanceProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestGoodInstanceProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestGroupingProvider1.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestGroupingProvider1.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestGroupingProvider2.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestGroupingProvider2.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestInstanceProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestInstanceProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestLocalizedProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestLocalizedProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestProviderLifecycleIndicationProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestProviderLifecycleIndicationProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestServerProfileProviders.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestServerProfileProviders.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestStressTestClient.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libTestStressTestClient.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUserContextTestProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUserContextTestProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libbenchmarkDefinition.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libbenchmarkDefinition.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libbenchmarkProviderModule.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libbenchmarkProviderModule.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiCWS_Util.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiCWS_Util.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiCppImpl.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiCppImpl.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiIndicationStressTestProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiIndicationStressTestProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiPerf_TestAssocClassProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiPerf_TestAssocClassProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiPerf_TestClassAProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiPerf_TestClassAProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiPerf_TestClassProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiPerf_TestClassProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiSampleInstanceProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiSampleInstanceProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiSampleMethodProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiSampleMethodProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestBrokerEncProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestBrokerEncProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestBrokerProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestBrokerProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestCMPIThreadProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestCMPIThreadProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestEnumerationProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestEnumerationProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestErrorProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestErrorProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestExecQueryProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestExecQueryProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestFail_1Provider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestFail_1Provider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestFail_2Provider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestFail_2Provider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestFail_3Provider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestFail_3Provider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestFail_4Provider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestFail_4Provider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestFail_5Provider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestFail_5Provider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestIndicationProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestIndicationProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestKeyReturnedProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestKeyReturnedProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestMethodProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiTestMethodProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiUtilLib.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcmpiUtilLib.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libgroupUtilLib.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libgroupUtilLib.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegauthentication.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegauthentication.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegclient.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegclient.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegcliutils.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegcliutils.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegcommon.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegcommon.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegcompiler.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegcompiler.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegconfig.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegconfig.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegcql.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegcql.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegdynlistener.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegdynlistener.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegexportclient.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegexportclient.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegexportserver.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegexportserver.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpeggeneral.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpeggeneral.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpeggetoopt.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpeggetoopt.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpeghandler.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpeghandler.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpeghandlerservice.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpeghandlerservice.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegindicationservice.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegindicationservice.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpeglistener.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpeglistener.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegmanagedclient.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegmanagedclient.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegpmrouter.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegpmrouter.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegpmservice.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegpmservice.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegprm.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegprm.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegprovider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegprovider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegprovidermanager.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegprovidermanager.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegquerycommon.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegquerycommon.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegqueryexpression.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegqueryexpression.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegrepository.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegrepository.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegsamplesspmodule.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegsamplesspmodule.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegserver.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegserver.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegservice.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegservice.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegslp.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegslp.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegslp_client.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegslp_client.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegwql.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegwql.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegwsmserver.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegwsmserver.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcimrsserver.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libcimrsserver.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegwebserver.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libpegwebserver.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libMeta_ClassProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libMeta_ClassProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_AccountProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_AccountProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_AccountManagementCapabilitiesProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_AccountManagementCapabilitiesProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_AccountOnSystemProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_AccountOnSystemProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_ApplicationSystemProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_ApplicationSystemProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_BIOSElementProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_BIOSElementProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_BIOSFeatureProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_BIOSFeatureProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_BIOSStringProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_BIOSStringProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_ClusterProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_ClusterProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_CurrentTimeProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_CurrentTimeProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_ComputerSystemProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_ComputerSystemProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_ComputerSystemPackageProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_ComputerSystemPackageProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_CreateDirectoryActionProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_CreateDirectoryActionProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_DatabaseServiceProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_DatabaseServiceProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_DatabaseSystemProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_DatabaseSystemProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_DeviceFileProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_DeviceFileProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_DirectoryProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_DirectoryProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_DiskDriveProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_DiskDriveProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_DiskPartitionProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_DiskPartitionProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_EthernetAdapterProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_EthernetAdapterProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_EthernetPortProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_EthernetPortProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_FileSystemProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_FileSystemProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_GroupProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_GroupProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_InstalledOSProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_InstalledOSProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_InstalledSoftwareElementProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_InstalledSoftwareElementProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_LocalFileSystemProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_LocalFileSystemProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_LogEntryProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_LogEntryProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_LogicalDiskProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_LogicalDiskProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_MessageLogProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_MessageLogProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_MountProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_MountProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_NFSProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_NFSProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_OSProcessProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_OSProcessProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_OperatingSystemProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_OperatingSystemProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_ProcessProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_ProcessProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_ProcessorProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_ProcessorProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_RemoteFileSystemProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_RemoteFileSystemProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_SoftwareElementProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_SoftwareElementProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_SoftwareFeatureProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_SoftwareFeatureProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_SystemBIOSProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_SystemBIOSProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_UsersAccessProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_UsersAccessProvider.so
	${LN} -sf ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_VirtualComputerSystemProvider.so.1 ${STAGEDIR}${PREFIX}/lib/pegasus/lib/libUNIX_VirtualComputerSystemProvider.so
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/SampleCMPIInstanceClient ${STAGEDIR}${PREFIX}/lib/pegasus/bin/SampleCMPIInstanceClient
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestAbstract ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestAbstract
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestAggregationOutputClient ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestAggregationOutputClient
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestAnonymousPipe ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestAnonymousPipe
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestAnonymousPipeChild ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestAnonymousPipeChild
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestArray ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestArray
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestAssocClassCache ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestAssocClassCache
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestAssocOperations ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestAssocOperations
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestAssocTable ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestAssocTable
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestAssociationTestClient ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestAssociationTestClient
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestAsyncCallback ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestAsyncCallback
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestAsyncQueue ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestAsyncQueue
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestAtomicInt ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestAtomicInt
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestAttribute ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestAttribute
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestAuditLogger ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestAuditLogger
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestAuthenticationInfo ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestAuthenticationInfo
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestAuthenticationManager ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestAuthenticationManager
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestAutoPtr ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestAutoPtr
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestBase64 ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestBase64
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestBasicAuthenticationHandler ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestBasicAuthenticationHandler
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestBenchmark ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestBenchmark
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestBinaryClient ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestBinaryClient
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestBuffer ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestBuffer
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCIMBuffer ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCIMBuffer
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCIMErrorClient ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCIMErrorClient
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCIMName ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCIMName
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCIMOMHandleTestProviderClient ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCIMOMHandleTestProviderClient
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCIMOMStatData ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCIMOMStatData
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCIMQueryCap ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCIMQueryCap
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCIMxmlIndicationHandlerDestination ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCIMxmlIndicationHandlerDestination
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCMPIAssociation ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCMPIAssociation
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCMPIBroker ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCMPIBroker
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCMPIBrokerEnc ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCMPIBrokerEnc
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCMPIEnumeration ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCMPIEnumeration
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCMPIError ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCMPIError
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCMPIIndicationSubscription ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCMPIIndicationSubscription
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCMPIInstance ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCMPIInstance
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCMPIInstanceExecQuery ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCMPIInstanceExecQuery
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCMPIKeyReturned ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCMPIKeyReturned
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCMPIMethod ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCMPIMethod
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCMPIProperty ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCMPIProperty
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCMPIThreadProvider ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCMPIThreadProvider
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCMPI_Fail_1 ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCMPI_Fail_1
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCMPI_Fail_2 ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCMPI_Fail_2
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCMPI_Fail_3 ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCMPI_Fail_3
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCMPI_Fail_5 ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCMPI_Fail_5
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCQL ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCQL
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCQLChainedIdentifier ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCQLChainedIdentifier
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCQLRegularExpression ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCQLRegularExpression
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCQLValue ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCQLValue
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCertClient ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCertClient
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestChgdHosts ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestChgdHosts
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestChunkingStressProviderClient ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestChunkingStressProviderClient
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCimError ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCimError
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCimsub ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCimsub
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCimsubInterOp ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCimsubInterOp
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestClassDecl ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestClassDecl
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestClassDeclRep ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestClassDeclRep
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestClient ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestClient
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCompAssoc ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCompAssoc
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCompare ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCompare
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestCompareRepositories ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestCompareRepositories
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestConfig ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestConfig
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestConfigFileHandler ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestConfigFileHandler
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestConfigManager ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestConfigManager
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestDateTime ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestDateTime
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestDeliveryRetry ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestDeliveryRetry
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestDir ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestDir
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestDisableEnable ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestDisableEnable
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestDisableEnable2 ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestDisableEnable2
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestDynListener ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestDynListener
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestDynamicLibrary ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestDynamicLibrary
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestEmbeddedInstance ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestEmbeddedInstance
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestEnumerateClassNames ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestEnumerateClassNames
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestExecutor ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestExecutor
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestExportClient ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestExportClient
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestFaultyInstanceProviderClient ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestFaultyInstanceProviderClient
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestFileHandler ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestFileHandler
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestFileLock ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestFileLock
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestFileSystem ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestFileSystem
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestFlavor ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestFlavor
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestFormatter ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestFormatter
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestG11n ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestG11n
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestGroupMethodProvider ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestGroupMethodProvider
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestHTTPExportResponseDecoder ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestHTTPExportResponseDecoder
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestHTTPMessage ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestHTTPMessage
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestHandlerTable ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestHandlerTable
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestHashTable ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestHashTable
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestHostAddress ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestHostAddress
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestHostLocator ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestHostLocator
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestIDFactory ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestIDFactory
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestIdleConnectionTimeout ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestIdleConnectionTimeout
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestIndicationProcess ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestIndicationProcess
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestIndicationService ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestIndicationService
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestIndicationStressTest ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestIndicationStressTest
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestIndicationsCount ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestIndicationsCount
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestInheritanceTree ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestInheritanceTree
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestInst ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestInst
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestInstAlias ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestInstAlias
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestInstanceDecl ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestInstanceDecl
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestInstanceProviderClient ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestInstanceProviderClient
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestInterop ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestInterop
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestL10N ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestL10N
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestList ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestList
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestListenAddress ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestListenAddress
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestListener ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestListener
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestLocalAuthFile ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestLocalAuthFile
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestLocalAuthenticationHandler ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestLocalAuthenticationHandler
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestLogger ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestLogger
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestMCCATestClient ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestMCCATestClient
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestMessageQueue ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestMessageQueue
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestMessageQueueService ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestMessageQueueService
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestMessageSerializer ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestMessageSerializer
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestMethod ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestMethod
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestMethodTestProvider ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestMethodTestProvider
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestModelWalkStressClient ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestModelWalkStressClient
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestModuleController ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestModuleController
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestMutex ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestMutex
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestNameSpace ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestNameSpace
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestNameSpaceManager ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestNameSpaceManager
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestOOPModuleFailure ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestOOPModuleFailure
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestObject ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestObject
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestObjectNormalizer ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestObjectNormalizer
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestOperationContext ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestOperationContext
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestOperationResponseHandler ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestOperationResponseHandler
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestOptionManager ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestOptionManager
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestOrderedSet ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestOrderedSet
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestParamValue ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestParamValue
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestParameter ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestParameter
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestParser ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestParser
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestParserCLI ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestParserCLI
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestPegClientAssociationClient ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestPegClientAssociationClient
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestPegClientClient ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestPegClientClient
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestPegClientClientStatistics ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestPegClientClientStatistics
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestPegClientDeleteNamespace ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestPegClientDeleteNamespace
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestPegClientEnumInstances ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestPegClientEnumInstances
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestPegClientInvokeMethod2 ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestPegClientInvokeMethod2
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestPegClientSLPClient ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestPegClientSLPClient
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestPegClientSSLCipherVerification ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestPegClientSSLCipherVerification
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestProcessIndication ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestProcessIndication
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestProperty ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestProperty
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestProvidMgrModifyInstances ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestProvidMgrModifyInstances
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestProvidMgrPluginMap ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestProvidMgrPluginMap
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestProviderLifecycleIndications ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestProviderLifecycleIndications
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestProviderManager ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestProviderManager
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestProviderMessageHandler ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestProviderMessageHandler
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestProviderModule ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestProviderModule
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestProviderName ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestProviderName
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestProviderStatus ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestProviderStatus
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestPrvRegMgrCreateInstances ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestPrvRegMgrCreateInstances
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestPrvRegMgrDeleteInstances ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestPrvRegMgrDeleteInstances
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestPrvRegMgrLookupIndicationConsumer ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestPrvRegMgrLookupIndicationConsumer
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestPrvRegMgrLookupIndicationProvider ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestPrvRegMgrLookupIndicationProvider
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestPrvRegMgrLookupInstanceProvider ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestPrvRegMgrLookupInstanceProvider
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestPrvRegMgrLookupMethodProvider ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestPrvRegMgrLookupMethodProvider
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestPrvRegMgrWildCardNamespaceNames ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestPrvRegMgrWildCardNamespaceNames
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestQualifier ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestQualifier
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestQualifierDecl ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestQualifierDecl
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestQualifierDeclCompile ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestQualifierDeclCompile
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestQualifierDeclRep ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestQualifierDeclRep
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestQualifierList ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestQualifierList
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestQueryContext ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestQueryContext
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestQueryExpression ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestQueryExpression
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestRT_Sample_Subscription ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestRT_Sample_Subscription
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestReference ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestReference
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestRepository ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestRepository
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestRepository2 ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestRepository2
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestRepositoryMix ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestRepositoryMix
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestResolve ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestResolve
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestResponseHandler ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestResponseHandler
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestSCMO ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestSCMO
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestSLP ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestSLP
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestSLP_SA ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestSLP_SA
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestScope ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestScope
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestServerProfile ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestServerProfile
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestSharedInheritanceTree ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestSharedInheritanceTree
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestSharedNameSpace ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestSharedNameSpace
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestSharedPtr ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestSharedPtr
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestSpinLock ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestSpinLock
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestStack ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestStack
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestStaticClient ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestStaticClient
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestStatisticalData ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestStatisticalData
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestStopwatch ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestStopwatch
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestStrToInstName ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestStrToInstName
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestStressTestController ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestStressTestController
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestString ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestString
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestStringConversion ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestStringConversion
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestSubscription ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestSubscription
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestSubscriptionKey ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestSubscriptionKey
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestSystem ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestSystem
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestThread ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestThread
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestThreadPool ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestThreadPool
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestTimeValue ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestTimeValue
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestToMof ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestToMof
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestTracer ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestTracer
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestUintArgs ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestUintArgs
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestUpgrade ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestUpgrade
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestUserContext ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestUserContext
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestValidateClass ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestValidateClass
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestValue ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestValue
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestWrapperStressClient ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestWrapperStressClient
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/ReliableIndicationDisableEnableTest ${STAGEDIR}${PREFIX}/lib/pegasus/bin/ReliableIndicationDisableEnableTest
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestWSMANExportClient ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestWSMANExportClient
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestWSMANRequestEncoder ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestWSMANRequestEncoder
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestwsmanIndicationHandlerDestination ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestwsmanIndicationHandlerDestination	
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestXmlDump ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestXmlDump
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestXmlParser ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestXmlParser
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestXmlPrint ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestXmlPrint
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestXmlReader ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestXmlReader
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestXmlWriter ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestXmlWriter
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestcreateRegistration ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestcreateRegistration
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/TestmodifyRegistration ${STAGEDIR}${PREFIX}/lib/pegasus/bin/TestmodifyRegistration
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/chksrc ${STAGEDIR}${PREFIX}/lib/pegasus/bin/chksrc
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/cimauth ${STAGEDIR}${PREFIX}/lib/pegasus/bin/cimauth
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/cimcli ${STAGEDIR}${PREFIX}/lib/pegasus/bin/cimcli
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/cimconfig ${STAGEDIR}${PREFIX}/lib/pegasus/bin/cimconfig
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/cimcrl ${STAGEDIR}${PREFIX}/lib/pegasus/bin/cimcrl
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/cimlistener ${STAGEDIR}${PREFIX}/lib/pegasus/bin/cimlistener
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/cimmof ${STAGEDIR}${PREFIX}/lib/pegasus/bin/cimmof
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/cimmofl ${STAGEDIR}${PREFIX}/lib/pegasus/bin/cimmofl
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/cimperf ${STAGEDIR}${PREFIX}/lib/pegasus/bin/cimperf
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/cimprovagt ${STAGEDIR}${PREFIX}/lib/pegasus/bin/cimprovagt
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/cimprovider ${STAGEDIR}${PREFIX}/lib/pegasus/bin/cimprovider
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/cimreparchive ${STAGEDIR}${PREFIX}/lib/pegasus/bin/cimreparchive
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/cimserver ${STAGEDIR}${PREFIX}/lib/pegasus/bin/cimserver
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/cimsub ${STAGEDIR}${PREFIX}/lib/pegasus/bin/cimsub
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/cimtrust ${STAGEDIR}${PREFIX}/lib/pegasus/bin/cimtrust
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/cnv2rootbundle ${STAGEDIR}${PREFIX}/lib/pegasus/bin/cnv2rootbundle
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/insertcrs ${STAGEDIR}${PREFIX}/lib/pegasus/bin/insertcrs
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/ipinfo ${STAGEDIR}${PREFIX}/lib/pegasus/bin/ipinfo
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/loadxml ${STAGEDIR}${PREFIX}/lib/pegasus/bin/loadxml
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/osinfo ${STAGEDIR}${PREFIX}/lib/pegasus/bin/osinfo
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/repupgrade ${STAGEDIR}${PREFIX}/lib/pegasus/bin/repupgrade
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/slp_attrreq ${STAGEDIR}${PREFIX}/lib/pegasus/bin/slp_attrreq
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/slp_query ${STAGEDIR}${PREFIX}/lib/pegasus/bin/slp_query
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/slp_srvreg ${STAGEDIR}${PREFIX}/lib/pegasus/bin/slp_srvreg
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/srv_reg ${STAGEDIR}${PREFIX}/lib/pegasus/bin/srv_reg
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/stripcrs ${STAGEDIR}${PREFIX}/lib/pegasus/bin/stripcrs
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/stripline ${STAGEDIR}${PREFIX}/lib/pegasus/bin/stripline
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/testCQLExecQuery ${STAGEDIR}${PREFIX}/lib/pegasus/bin/testCQLExecQuery
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/testExecQuery ${STAGEDIR}${PREFIX}/lib/pegasus/bin/testExecQuery
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/tomof ${STAGEDIR}${PREFIX}/lib/pegasus/bin/tomof
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/wbemexec ${STAGEDIR}${PREFIX}/lib/pegasus/bin/wbemexec
	${INSTALL_PROGRAM} ${PEGASUS_HOME}/bin/WSMANInvokeMethod ${STAGEDIR}${PREFIX}/lib/pegasus/bin/WSMANInvokeMethod	
	${INSTALL_SCRIPT} ${WRKDIR}/chksrc ${STAGEDIR}${PREFIX}/bin/chksrc
	${INSTALL_SCRIPT} ${WRKDIR}/cimauth ${STAGEDIR}${PREFIX}/bin/cimauth
	${INSTALL_SCRIPT} ${WRKDIR}/cimcli ${STAGEDIR}${PREFIX}/bin/cimcli
	${INSTALL_SCRIPT} ${WRKDIR}/cimconfig ${STAGEDIR}${PREFIX}/bin/cimconfig
	${INSTALL_SCRIPT} ${WRKDIR}/cimcrl ${STAGEDIR}${PREFIX}/bin/cimcrl
	${INSTALL_SCRIPT} ${WRKDIR}/cimlistener ${STAGEDIR}${PREFIX}/bin/cimlistener
	${INSTALL_SCRIPT} ${WRKDIR}/cimmof ${STAGEDIR}${PREFIX}/bin/cimmof
	${INSTALL_SCRIPT} ${WRKDIR}/cimmofl ${STAGEDIR}${PREFIX}/bin/cimmofl
	${INSTALL_SCRIPT} ${WRKDIR}/cimperf ${STAGEDIR}${PREFIX}/bin/cimperf
	${INSTALL_SCRIPT} ${WRKDIR}/cimprovagt ${STAGEDIR}${PREFIX}/bin/cimprovagt
	${INSTALL_SCRIPT} ${WRKDIR}/cimprovider ${STAGEDIR}${PREFIX}/bin/cimprovider
	${INSTALL_SCRIPT} ${WRKDIR}/cimreparchive ${STAGEDIR}${PREFIX}/bin/cimreparchive
	${INSTALL_SCRIPT} ${WRKDIR}/cimserver ${STAGEDIR}${PREFIX}/bin/cimserver
	${INSTALL_SCRIPT} ${WRKDIR}/cimsub ${STAGEDIR}${PREFIX}/bin/cimsub
	${INSTALL_SCRIPT} ${WRKDIR}/cimtrust ${STAGEDIR}${PREFIX}/bin/cimtrust
	${INSTALL_SCRIPT} ${WRKDIR}/cnv2rootbundle ${STAGEDIR}${PREFIX}/bin/cnv2rootbundle
	${INSTALL_SCRIPT} ${WRKDIR}/insertcrs ${STAGEDIR}${PREFIX}/bin/insertcrs
	${INSTALL_SCRIPT} ${WRKDIR}/ipinfo ${STAGEDIR}${PREFIX}/bin/cimipinfo
	${INSTALL_SCRIPT} ${WRKDIR}/loadxml ${STAGEDIR}${PREFIX}/bin/cimloadxml
	${INSTALL_SCRIPT} ${WRKDIR}/osinfo ${STAGEDIR}${PREFIX}/bin/cimosinfo
	${INSTALL_SCRIPT} ${WRKDIR}/repupgrade ${STAGEDIR}${PREFIX}/bin/cimrepupgrade
	${INSTALL_SCRIPT} ${WRKDIR}/slp_attrreq ${STAGEDIR}${PREFIX}/bin/slp_attrreq
	${INSTALL_SCRIPT} ${WRKDIR}/slp_query ${STAGEDIR}${PREFIX}/bin/slp_query
	${INSTALL_SCRIPT} ${WRKDIR}/slp_srvreg ${STAGEDIR}${PREFIX}/bin/slp_srvreg
	${INSTALL_SCRIPT} ${WRKDIR}/srv_reg ${STAGEDIR}${PREFIX}/bin/srv_reg
	${INSTALL_SCRIPT} ${WRKDIR}/stripcrs ${STAGEDIR}${PREFIX}/bin/stripcrs
	${INSTALL_SCRIPT} ${WRKDIR}/stripline ${STAGEDIR}${PREFIX}/bin/cimstripline
	${INSTALL_SCRIPT} ${WRKDIR}/testCQLExecQuery ${STAGEDIR}${PREFIX}/bin/testCQLExecQuery
	${INSTALL_SCRIPT} ${WRKDIR}/testExecQuery ${STAGEDIR}${PREFIX}/bin/testExecQuery
	${INSTALL_SCRIPT} ${WRKDIR}/tomof ${STAGEDIR}${PREFIX}/bin/tomof
	${INSTALL_SCRIPT} ${WRKDIR}/wbemexec ${STAGEDIR}${PREFIX}/bin/wbemexec
	${INSTALL_SCRIPT} ${WRKDIR}/cimserverd ${STAGEDIR}${PREFIX}/etc/rc.d/cimserver
	${INSTALL_DATA} ${PEGASUS_HOME}/server.pem ${STAGEDIR}${PREFIX}/etc/pegasus/server.pem
	${INSTALL_DATA} ${PEGASUS_HOME}/cert.pem ${STAGEDIR}${PREFIX}/etc/pegasus/cert.pem
	${INSTALL_DATA} ${PEGASUS_HOME}/cimserver.rnd ${STAGEDIR}${PREFIX}/etc/pegasus/cimserver.rnd
	${INSTALL_DATA} ${PEGASUS_HOME}/client.pem ${STAGEDIR}${PREFIX}/etc/pegasus/client.pem
	${INSTALL_DATA} ${PEGASUS_HOME}/client_cert.pem ${STAGEDIR}${PREFIX}/etc/pegasus/client_cert.pem
	${INSTALL_DATA} ${PEGASUS_HOME}/client_file.pem	${STAGEDIR}${PREFIX}/etc/pegasus/client_file.pem
	${INSTALL_DATA} ${PEGASUS_HOME}/file.pem ${STAGEDIR}${PREFIX}/etc/pegasus/file.pem
	${INSTALL_DATA} ${PEGASUS_HOME}/ssl.cnf ${STAGEDIR}${PREFIX}/etc/pegasus/ssl.cnf
	${INSTALL_DATA} ${PEGASUS_HOME}/ssl.rnd ${STAGEDIR}${PREFIX}/etc/pegasus/ssl.rnd
	${INSTALL_DATA}	${WRKDIR}/cimserver_planned.conf ${STAGEDIR}${PREFIX}/etc/pegasus/cimserver_planned.conf
	${INSTALL_DATA} ${WRKDIR}/cimserver_planned.conf ${STAGEDIR}${PREFIX}/etc/pegasus/cimserver_current.conf
	${INSTALL_DATA} ${WRKDIR}/cimserver_planned.conf ${STAGEDIR}${PREFIX}/etc/pegasus/cimserver.conf
	${LN} -sf ${STAGEDIR}${PREFIX}/etc/pegasus/cimserver.conf ${STAGEDIR}${PREFIX}/lib/pegasus/cimserver_current.conf
	@(cd ${PEGASUS_HOME} && ${TAR} pczf repository-CIM236.tar.gz repository)
	${MKDIR} ${STAGEDIR}${PREFIX}/share/pegasus
	${INSTALL_DATA} ${PEGASUS_HOME}/repository-CIM236.tar.gz ${STAGEDIR}${PREFIX}/share/pegasus/repository-CIM236.tar.gz
	@(cd ${PEGASUS_HOME}/www && ${CP} -R * ${STAGEDIR}${PREFIX}/www/pegasus)

.include <bsd.port.post.mk>
