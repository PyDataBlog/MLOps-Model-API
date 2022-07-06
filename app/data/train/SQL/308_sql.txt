USE [VotingInfo]
DECLARE @SchemaName as nvarchar(20); SET @SchemaName = 'Auth';




-------------------------------------------------------------------
-- LOGIC CODE BELOW, EDIT SCHEMA NAME ABOVE
-------------------------------------------------------------------

IF DB_NAME() IN ('master', 'msdb', 'model', 'distribution')
BEGIN
	RAISERROR('Not for use on system databases', 16, 1)
	GOTO Done
END

SET NOCOUNT ON

DECLARE @DropStatement nvarchar(4000)
DECLARE @SequenceNumber int
DECLARE @LastError int
DECLARE @TablesDropped int

DECLARE DropStatements CURSOR LOCAL FAST_FORWARD READ_ONLY FOR

-- Stored Procedures
	SELECT	1 AS SequenceNumber,
			N'DROP PROCEDURE ' + QUOTENAME(ROUTINE_SCHEMA) + N'.' + QUOTENAME(ROUTINE_NAME) AS DropStatement
	FROM	INFORMATION_SCHEMA.ROUTINES
	WHERE	ROUTINE_TYPE = N'PROCEDURE'
	AND		ROUTINE_SCHEMA = @SchemaName
	AND		OBJECTPROPERTY( OBJECT_ID(QUOTENAME(ROUTINE_SCHEMA) + N'.' + QUOTENAME(ROUTINE_NAME)), 'IsMSShipped') = 0
UNION ALL
-- Functions
	SELECT	2 AS SequenceNumber,
			N'DROP FUNCTION ' + QUOTENAME(ROUTINE_SCHEMA) + N'.' + QUOTENAME(ROUTINE_NAME) AS DropStatement
	FROM	INFORMATION_SCHEMA.ROUTINES
	WHERE	ROUTINE_TYPE = N'FUNCTION'
	AND		ROUTINE_SCHEMA = @SchemaName
	AND		OBJECTPROPERTY( OBJECT_ID(QUOTENAME(ROUTINE_SCHEMA) + N'.' + QUOTENAME(ROUTINE_NAME)), 'IsMSShipped') = 0
UNION ALL
-- Forgeign Keys
	SELECT	3 AS SequenceNumber, 
			N'ALTER TABLE ' + QUOTENAME(TABLE_SCHEMA) + N'.' + QUOTENAME(TABLE_NAME) + N' DROP CONSTRAINT ' + CONSTRAINT_NAME AS DropStatement
	FROM	INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE	CONSTRAINT_TYPE = N'FOREIGN KEY'
	AND		CONSTRAINT_SCHEMA = @SchemaName
UNION ALL
-- Indexes
	SELECT	4 AS SequenceNumber,
			N'DROP INDEX ' + QUOTENAME(IX.[Name]) + N' ON ' + QUOTENAME(S.[Name]) + N'.' + QUOTENAME(T.[Name]) + N' WITH ( ONLINE = OFF )' AS DropStatement
	FROM	sys.indexes IX
	INNER JOIN sys.objects T ON IX.Object_Id = T.Object_Id
	INNER JOIN sys.schemas S ON T.Schema_Id = S.Schema_Id
	WHERE	IX.[Name] LIKE 'IX_%'
	AND		S.[Name] = @SchemaName	
UNION ALL
-- Views
	SELECT	5 AS SequenceNumber,
			N'DROP VIEW ' + QUOTENAME(TABLE_SCHEMA) + N'.' + QUOTENAME(TABLE_NAME) AS DropStatement
	FROM	INFORMATION_SCHEMA.TABLES
	WHERE	TABLE_TYPE = N'VIEW' 
	AND		TABLE_SCHEMA = @SchemaName	
	AND		OBJECTPROPERTY( OBJECT_ID(QUOTENAME(TABLE_SCHEMA) + N'.' + QUOTENAME(TABLE_NAME)), 'IsMSShipped') = 0

OPEN DropStatements
WHILE 1 = 1
BEGIN
	FETCH NEXT FROM DropStatements INTO @SequenceNumber, @DropStatement
	IF @@FETCH_STATUS = -1 BREAK
	BEGIN
		RAISERROR('%s', 0, 1, @DropStatement) WITH NOWAIT
		EXECUTE sp_ExecuteSQL @DropStatement
		SET @LastError = @@ERROR
		IF @LastError > 0
		BEGIN
			RAISERROR('Script terminated due to unexpected error', 16, 1)
			GOTO Done
		END
	END
END

CLOSE DropStatements
DEALLOCATE DropStatements

Done:
GO--
USE [VotingInfo]
DECLARE @SchemaName as nvarchar(20); SET @SchemaName = 'Client';




-------------------------------------------------------------------
-- LOGIC CODE BELOW, EDIT SCHEMA NAME ABOVE
-------------------------------------------------------------------

IF DB_NAME() IN ('master', 'msdb', 'model', 'distribution')
BEGIN
	RAISERROR('Not for use on system databases', 16, 1)
	GOTO Done
END

SET NOCOUNT ON

DECLARE @DropStatement nvarchar(4000)
DECLARE @SequenceNumber int
DECLARE @LastError int
DECLARE @TablesDropped int

DECLARE DropStatements CURSOR LOCAL FAST_FORWARD READ_ONLY FOR

-- Stored Procedures
	SELECT	1 AS SequenceNumber,
			N'DROP PROCEDURE ' + QUOTENAME(ROUTINE_SCHEMA) + N'.' + QUOTENAME(ROUTINE_NAME) AS DropStatement
	FROM	INFORMATION_SCHEMA.ROUTINES
	WHERE	ROUTINE_TYPE = N'PROCEDURE'
	AND		ROUTINE_SCHEMA = @SchemaName
	AND		OBJECTPROPERTY( OBJECT_ID(QUOTENAME(ROUTINE_SCHEMA) + N'.' + QUOTENAME(ROUTINE_NAME)), 'IsMSShipped') = 0
UNION ALL
-- Functions
	SELECT	2 AS SequenceNumber,
			N'DROP FUNCTION ' + QUOTENAME(ROUTINE_SCHEMA) + N'.' + QUOTENAME(ROUTINE_NAME) AS DropStatement
	FROM	INFORMATION_SCHEMA.ROUTINES
	WHERE	ROUTINE_TYPE = N'FUNCTION'
	AND		ROUTINE_SCHEMA = @SchemaName
	AND		OBJECTPROPERTY( OBJECT_ID(QUOTENAME(ROUTINE_SCHEMA) + N'.' + QUOTENAME(ROUTINE_NAME)), 'IsMSShipped') = 0
UNION ALL
-- Forgeign Keys
	SELECT	3 AS SequenceNumber, 
			N'ALTER TABLE ' + QUOTENAME(TABLE_SCHEMA) + N'.' + QUOTENAME(TABLE_NAME) + N' DROP CONSTRAINT ' + CONSTRAINT_NAME AS DropStatement
	FROM	INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE	CONSTRAINT_TYPE = N'FOREIGN KEY'
	AND		CONSTRAINT_SCHEMA = @SchemaName
UNION ALL
-- Indexes
	SELECT	4 AS SequenceNumber,
			N'DROP INDEX ' + QUOTENAME(IX.[Name]) + N' ON ' + QUOTENAME(S.[Name]) + N'.' + QUOTENAME(T.[Name]) + N' WITH ( ONLINE = OFF )' AS DropStatement
	FROM	sys.indexes IX
	INNER JOIN sys.objects T ON IX.Object_Id = T.Object_Id
	INNER JOIN sys.schemas S ON T.Schema_Id = S.Schema_Id
	WHERE	IX.[Name] LIKE 'IX_%'
	AND		S.[Name] = @SchemaName	
UNION ALL
-- Views
	SELECT	5 AS SequenceNumber,
			N'DROP VIEW ' + QUOTENAME(TABLE_SCHEMA) + N'.' + QUOTENAME(TABLE_NAME) AS DropStatement
	FROM	INFORMATION_SCHEMA.TABLES
	WHERE	TABLE_TYPE = N'VIEW' 
	AND		TABLE_SCHEMA = @SchemaName	
	AND		OBJECTPROPERTY( OBJECT_ID(QUOTENAME(TABLE_SCHEMA) + N'.' + QUOTENAME(TABLE_NAME)), 'IsMSShipped') = 0

OPEN DropStatements
WHILE 1 = 1
BEGIN
	FETCH NEXT FROM DropStatements INTO @SequenceNumber, @DropStatement
	IF @@FETCH_STATUS = -1 BREAK
	BEGIN
		RAISERROR('%s', 0, 1, @DropStatement) WITH NOWAIT
		EXECUTE sp_ExecuteSQL @DropStatement
		SET @LastError = @@ERROR
		IF @LastError > 0
		BEGIN
			RAISERROR('Script terminated due to unexpected error', 16, 1)
			GOTO Done
		END
	END
END

CLOSE DropStatements
DEALLOCATE DropStatements

Done:
GO--
USE [VotingInfo]
DECLARE @SchemaName as nvarchar(20); SET @SchemaName = 'Data';




-------------------------------------------------------------------
-- LOGIC CODE BELOW, EDIT SCHEMA NAME ABOVE
-------------------------------------------------------------------

IF DB_NAME() IN ('master', 'msdb', 'model', 'distribution')
BEGIN
	RAISERROR('Not for use on system databases', 16, 1)
	GOTO Done
END

SET NOCOUNT ON

DECLARE @DropStatement nvarchar(4000)
DECLARE @SequenceNumber int
DECLARE @LastError int
DECLARE @TablesDropped int

DECLARE DropStatements CURSOR LOCAL FAST_FORWARD READ_ONLY FOR

-- Stored Procedures
	SELECT	1 AS SequenceNumber,
			N'DROP PROCEDURE ' + QUOTENAME(ROUTINE_SCHEMA) + N'.' + QUOTENAME(ROUTINE_NAME) AS DropStatement
	FROM	INFORMATION_SCHEMA.ROUTINES
	WHERE	ROUTINE_TYPE = N'PROCEDURE'
	AND		ROUTINE_SCHEMA = @SchemaName
	AND		OBJECTPROPERTY( OBJECT_ID(QUOTENAME(ROUTINE_SCHEMA) + N'.' + QUOTENAME(ROUTINE_NAME)), 'IsMSShipped') = 0
UNION ALL
-- Functions
	SELECT	2 AS SequenceNumber,
			N'DROP FUNCTION ' + QUOTENAME(ROUTINE_SCHEMA) + N'.' + QUOTENAME(ROUTINE_NAME) AS DropStatement
	FROM	INFORMATION_SCHEMA.ROUTINES
	WHERE	ROUTINE_TYPE = N'FUNCTION'
	AND		ROUTINE_SCHEMA = @SchemaName
	AND		OBJECTPROPERTY( OBJECT_ID(QUOTENAME(ROUTINE_SCHEMA) + N'.' + QUOTENAME(ROUTINE_NAME)), 'IsMSShipped') = 0
UNION ALL
-- Forgeign Keys
	SELECT	3 AS SequenceNumber, 
			N'ALTER TABLE ' + QUOTENAME(TABLE_SCHEMA) + N'.' + QUOTENAME(TABLE_NAME) + N' DROP CONSTRAINT ' + CONSTRAINT_NAME AS DropStatement
	FROM	INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE	CONSTRAINT_TYPE = N'FOREIGN KEY'
	AND		CONSTRAINT_SCHEMA = @SchemaName
UNION ALL
-- Indexes
	SELECT	4 AS SequenceNumber,
			N'DROP INDEX ' + QUOTENAME(IX.[Name]) + N' ON ' + QUOTENAME(S.[Name]) + N'.' + QUOTENAME(T.[Name]) + N' WITH ( ONLINE = OFF )' AS DropStatement
	FROM	sys.indexes IX
	INNER JOIN sys.objects T ON IX.Object_Id = T.Object_Id
	INNER JOIN sys.schemas S ON T.Schema_Id = S.Schema_Id
	WHERE	IX.[Name] LIKE 'IX_%'
	AND		S.[Name] = @SchemaName	
UNION ALL
-- Views
	SELECT	5 AS SequenceNumber,
			N'DROP VIEW ' + QUOTENAME(TABLE_SCHEMA) + N'.' + QUOTENAME(TABLE_NAME) AS DropStatement
	FROM	INFORMATION_SCHEMA.TABLES
	WHERE	TABLE_TYPE = N'VIEW' 
	AND		TABLE_SCHEMA = @SchemaName	
	AND		OBJECTPROPERTY( OBJECT_ID(QUOTENAME(TABLE_SCHEMA) + N'.' + QUOTENAME(TABLE_NAME)), 'IsMSShipped') = 0

OPEN DropStatements
WHILE 1 = 1
BEGIN
	FETCH NEXT FROM DropStatements INTO @SequenceNumber, @DropStatement
	IF @@FETCH_STATUS = -1 BREAK
	BEGIN
		RAISERROR('%s', 0, 1, @DropStatement) WITH NOWAIT
		EXECUTE sp_ExecuteSQL @DropStatement
		SET @LastError = @@ERROR
		IF @LastError > 0
		BEGIN
			RAISERROR('Script terminated due to unexpected error', 16, 1)
			GOTO Done
		END
	END
END

CLOSE DropStatements
DEALLOCATE DropStatements

Done:
GO--
DECLARE @SchemaName as nvarchar(20); SET @SchemaName = 'Auth';


-------------------------------------------------------------------
-- LOGIC CODE BELOW, EDIT SCHEMA NAME ABOVE
-------------------------------------------------------------------

IF NOT EXISTS (select * FROM sys.schemas WHERE name = @SchemaName)
	BEGIN   
		EXEC('CREATE SCHEMA [' + @SchemaName + ']');
		EXEC('GRANT EXECUTE ON SCHEMA::[' + @SchemaName + '] TO db_executor');
	END
GO--
DECLARE @SchemaName as nvarchar(20); SET @SchemaName = 'Client';


-------------------------------------------------------------------
-- LOGIC CODE BELOW, EDIT SCHEMA NAME ABOVE
-------------------------------------------------------------------

IF NOT EXISTS (select * FROM sys.schemas WHERE name = @SchemaName)
	BEGIN   
		EXEC('CREATE SCHEMA [' + @SchemaName + ']');
		EXEC('GRANT EXECUTE ON SCHEMA::[' + @SchemaName + '] TO db_executor');
	END
GO--
DECLARE @SchemaName as nvarchar(20); SET @SchemaName = 'Data';


-------------------------------------------------------------------
-- LOGIC CODE BELOW, EDIT SCHEMA NAME ABOVE
-------------------------------------------------------------------

IF NOT EXISTS (select * FROM sys.schemas WHERE name = @SchemaName)
	BEGIN   
		EXEC('CREATE SCHEMA [' + @SchemaName + ']');
		EXEC('GRANT EXECUTE ON SCHEMA::[' + @SchemaName + '] TO db_executor');
	END
GO--
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[Auth].[TableVersion]') AND type in (N'U'))
BEGIN
	CREATE TABLE [Auth].[TableVersion](
		[TableId]		[int]			IDENTITY(1,1) NOT NULL,
		[TableName]		[varchar](150)	NOT NULL,
		[Version]		[int]			NOT NULL DEFAULT ((0)),
		CONSTRAINT [PK_TableVersion] PRIMARY KEY CLUSTERED ([TableId] ASC)
	)
	
	INSERT INTO [Auth].[TableVersion] ([TableName],[Version]) VALUES('[Auth].[TableVersion]',0)
END
GO--
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[Auth].[Permission]') AND type in (N'U'))
BEGIN
	CREATE TABLE [Auth].[Permission](
		[PermissionId] [int] IDENTITY(1,1) NOT NULL, --NOTE: This will probably not end up lining up with the enum value, go by name instead.
		[PermissionName] [varchar](100) NOT NULL, --NOTE: This should be the same as the auto generated enum.
		[Title] [varchar](150) NOT NULL, --NOTE: This is a human readable title.
		[IsRead] [bit] NOT NULL,
		CONSTRAINT [PK_Permission] PRIMARY KEY CLUSTERED ([PermissionId] ASC)
	)  
	INSERT INTO [Auth].[TableVersion]([TableName],[Version]) VALUES('[Auth].[Permission]', 0)
END

GO--
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[Auth].[Role]') AND type in (N'U'))
BEGIN
	CREATE TABLE [Auth].[Role](
		[RoleId] [int] IDENTITY(1,1) NOT NULL,
		[Title] [varchar](50) NOT NULL,
		[Description] [varchar](max) NOT NULL,
		[IsActive] [bit] NOT NULL,
		[ApplyToAnon] [bit] NOT NULL,		-- Use this flag to automatically have this apply to anonymous
		[ApplyToAllUsers] [bit] NOT NULL,	-- Use this flag to automatically have this apply to all users
		[PreventAddingUsers] [bit] NOT NULL,  -- System admin means do not allow addition of any users by proc
		[WINSID] [varchar](50) NULL,		-- This winsid is the id of a group, and can be used by an AD Sync operation to automatically add users to the role.
		CONSTRAINT [PK_Role] PRIMARY KEY CLUSTERED ([RoleId] ASC)
	)  
	INSERT INTO [Auth].[TableVersion]([TableName],[Version]) VALUES('[Auth].[Role]', 0)
END

GO--
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[Auth].[RolePermission]') AND type in (N'U'))
BEGIN
	CREATE TABLE [Auth].[RolePermission](
		[RolePermissionId] [int] IDENTITY(1,1) NOT NULL,
		[RoleId] [int] NOT NULL,
		[PermissionId] [int] NOT NULL,
		CONSTRAINT [PK_RolePermission] PRIMARY KEY CLUSTERED ([RolePermissionId] ASC)
	)  
	INSERT INTO [Auth].[TableVersion]([TableName],[Version]) VALUES('[Auth].[RolePermission]', 0)
END

GO--
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[Auth].[User]') AND type in (N'U'))
BEGIN
	CREATE TABLE [Auth].[User](
		[UserId] [int] IDENTITY(1,1) NOT NULL,
		[UserName] [varchar](50) NOT NULL,
		[Password] [binary](16) NOT NULL,
		[DisplayName] [varchar](50) NOT NULL,
		[Email] [varchar](100) NOT NULL,
		[AuthToken] [uniqueidentifier] NOT NULL,
		[UserToken] [uniqueidentifier] NOT NULL,
		[FailedLogins] [int] NOT NULL,
		[IsActive] [bit] NOT NULL,
		[WINSID] [varchar](50) NULL,
		CONSTRAINT [PK_User] PRIMARY KEY CLUSTERED ([UserId] ASC)
	)  
	INSERT INTO [Auth].[TableVersion]([TableName],[Version]) VALUES('[Auth].[User]', 0)
END

GO--
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[Auth].[UserRole]') AND type in (N'U'))
BEGIN
	CREATE TABLE [Auth].[UserRole](
		[UserRoleId] [int] IDENTITY(1,1) NOT NULL,
		[UserId] [int] NOT NULL,
		[RoleId] [int] NOT NULL,
		CONSTRAINT [PK_UserRole] PRIMARY KEY CLUSTERED ([UserRoleId] ASC)
	)  
	INSERT INTO [Auth].[TableVersion]([TableName],[Version]) VALUES('[Auth].[UserRole]', 0)
END

GO--
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[Client].[TableVersion]') AND type in (N'U'))
BEGIN
	CREATE TABLE [Client].[TableVersion](
		[TableId]		[int]			IDENTITY(1,1) NOT NULL,
		[TableName]		[varchar](150)	NOT NULL,
		[Version]		[int]			NOT NULL DEFAULT ((0)),
		CONSTRAINT [PK_TableVersion] PRIMARY KEY CLUSTERED ([TableId] ASC)
	)
	
	INSERT INTO [Client].[TableVersion] ([TableName],[Version]) VALUES('[Client].[TableVersion]',0)
END
GO--
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[Data].[TableVersion]') AND type in (N'U'))
BEGIN
	CREATE TABLE [Data].[TableVersion](
		[TableId]		[int]			IDENTITY(1,1) NOT NULL,
		[TableName]		[varchar](150)	NOT NULL,
		[Version]		[int]			NOT NULL DEFAULT ((0)),
		CONSTRAINT [PK_TableVersion] PRIMARY KEY CLUSTERED ([TableId] ASC)
	)
	
	INSERT INTO [Data].[TableVersion] ([TableName],[Version]) VALUES('[Data].[TableVersion]',0)
END
GO--
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[Data].[Candidate]') AND type in (N'U'))
BEGIN
	CREATE TABLE [Data].[Candidate](
		[CandidateId] int IDENTITY(1,1) NOT NULL,
		[UserId] int  NOT NULL,
		[ContentInspectionId] int  NOT NULL,
		[LocationId] int  NOT NULL,
		[OrganizationId] int  NOT NULL,
		[CandidateName] varchar(150)  NOT NULL,
		CONSTRAINT [PK_Candidate] PRIMARY KEY CLUSTERED ([CandidateId])
	)  
	INSERT INTO [Data].[TableVersion]([TableName],[Version]) VALUES('[Data].[Candidate]', 0)
END

GO--
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[Data].[CandidateMetaData]') AND type in (N'U'))
BEGIN
	CREATE TABLE [Data].[CandidateMetaData](
		[CandidateMetaDataId] int IDENTITY(1,1) NOT NULL,
		[ContentInspectionId] int  NOT NULL,
		[CandidateId] int  NOT NULL,
		[MetaDataId] int  NOT NULL,
		[MetaDataValue] varchar(max)  NOT NULL,
		CONSTRAINT [PK_CandidateMetaData] PRIMARY KEY CLUSTERED ([CandidateMetaDataId])
	)  
	INSERT INTO [Data].[TableVersion]([TableName],[Version]) VALUES('[Data].[CandidateMetaData]', 0)
END

GO--
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[Data].[ContentInspection]') AND type in (N'U'))
BEGIN
	CREATE TABLE [Data].[ContentInspection](
		[ContentInspectionId] int IDENTITY(1,1) NOT NULL,
		[IsArchived] bit  NOT NULL,
		[IsBeingProposed] bit  NOT NULL,
		[ProposedByUserId] int  NOT NULL,
		[ConfirmedByUserId] int  NOT NULL,
		[FalseInfoCount] int  NOT NULL,
		[TrueInfoCount] int  NOT NULL,
		[AdminInpsected] bit  NOT NULL,
		[DateLastChecked] datetime  NOT NULL,
		[SourceUrl] varchar(250)  NOT NULL,
		CONSTRAINT [PK_ContentInspection] PRIMARY KEY CLUSTERED ([ContentInspectionId])
	)  
	INSERT INTO [Data].[TableVersion]([TableName],[Version]) VALUES('[Data].[ContentInspection]', 0)
END

GO--
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[Data].[Election]') AND type in (N'U'))
BEGIN
	CREATE TABLE [Data].[Election](
		[ElectionId] int IDENTITY(1,1) NOT NULL,
		[ContentInspectionId] int  NOT NULL,
		[ElectionLevelId] int  NOT NULL,
		[LocationId] int  NOT NULL,
		[VotingDate] datetime  NOT NULL,
		CONSTRAINT [PK_Election] PRIMARY KEY CLUSTERED ([ElectionId])
	)  
	INSERT INTO [Data].[TableVersion]([TableName],[Version]) VALUES('[Data].[Election]', 0)
END

GO--
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[Data].[ElectionCandidate]') AND type in (N'U'))
BEGIN
	CREATE TABLE [Data].[ElectionCandidate](
		[ElectionCandidateId] int IDENTITY(1,1) NOT NULL,
		[ContentInspectionId] int  NOT NULL,
		[CandidateId] int  NOT NULL,
		[ElectionId] int  NOT NULL,
		[IsWinner] bit  NOT NULL,
		[ReportedVoteCount] bit  NOT NULL,
		CONSTRAINT [PK_ElectionCandidate] PRIMARY KEY CLUSTERED ([ElectionCandidateId])
	)  
	INSERT INTO [Data].[TableVersion]([TableName],[Version]) VALUES('[Data].[ElectionCandidate]', 0)
END

GO--
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[Data].[ElectionLevel]') AND type in (N'U'))
BEGIN
	CREATE TABLE [Data].[ElectionLevel](
		[ElectionLevelId] int IDENTITY(1,1) NOT NULL,
		[ContentInspectionId] int  NOT NULL,
		[ElectionLevelTitle] varchar(150)  NOT NULL,
		CONSTRAINT [PK_ElectionLevel] PRIMARY KEY CLUSTERED ([ElectionLevelId])
	)  
	INSERT INTO [Data].[TableVersion]([TableName],[Version]) VALUES('[Data].[ElectionLevel]', 0)
END

GO--
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[Data].[ElectionLevelMetaDataXref]') AND type in (N'U'))
BEGIN
	CREATE TABLE [Data].[ElectionLevelMetaDataXref](
		[ElectionLevelMetaDataXrefId] int IDENTITY(1,1) NOT NULL,
		[ContentInspectionId] int  NOT NULL,
		[ElectionLevelId] int  NOT NULL,
		[MetaDataId] nchar(10)  NULL,
		CONSTRAINT [PK_ElectionLevelMetaDataXref] PRIMARY KEY CLUSTERED ([ElectionLevelMetaDataXrefId])
	)  
	INSERT INTO [Data].[TableVersion]([TableName],[Version]) VALUES('[Data].[ElectionLevelMetaDataXref]', 0)
END

GO--
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[Data].[Location]') AND type in (N'U'))
BEGIN
	CREATE TABLE [Data].[Location](
		[LocationId] int IDENTITY(1,1) NOT NULL,
		[ContentInspectionId] int  NOT NULL,
		[LocationName] varchar(150)  NOT NULL,
		CONSTRAINT [PK_Location] PRIMARY KEY CLUSTERED ([LocationId])
	)  
	INSERT INTO [Data].[TableVersion]([TableName],[Version]) VALUES('[Data].[Location]', 0)
END

GO--
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[Data].[MetaData]') AND type in (N'U'))
BEGIN
	CREATE TABLE [Data].[MetaData](
		[MetaDataId] int IDENTITY(1,1) NOT NULL,
		[ContentInspectionId] int  NOT NULL,
		[MetaDataName] varchar(150)  NOT NULL,
		[IsRequired] bit  NOT NULL,
		[AppliesAtAllLevels] bit  NOT NULL,
		[AppliesToCandidates] bit  NOT NULL,
		[AppliesToOrganizations] bit  NOT NULL,
		CONSTRAINT [PK_MetaData] PRIMARY KEY CLUSTERED ([MetaDataId])
	)  
	INSERT INTO [Data].[TableVersion]([TableName],[Version]) VALUES('[Data].[MetaData]', 0)
END

GO--
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[Data].[Organization]') AND type in (N'U'))
BEGIN
	CREATE TABLE [Data].[Organization](
		[OrganizationId] int IDENTITY(1,1) NOT NULL,
		[ContentInspectionId] int  NOT NULL,
		[OrganizationName] varchar(250)  NOT NULL,
		CONSTRAINT [PK_Organization] PRIMARY KEY CLUSTERED ([OrganizationId])
	)  
	INSERT INTO [Data].[TableVersion]([TableName],[Version]) VALUES('[Data].[Organization]', 0)
END

GO--
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[Data].[OrganizationMetaData]') AND type in (N'U'))
BEGIN
	CREATE TABLE [Data].[OrganizationMetaData](
		[OrganizationMetaDataId] int IDENTITY(1,1) NOT NULL,
		[ContentInspectionId] int  NOT NULL,
		[OrganizationId] int  NOT NULL,
		[MetaDataId] int  NOT NULL,
		[MetaDataValue] varchar(max)  NOT NULL,
		CONSTRAINT [PK_OrganizationMetaData] PRIMARY KEY CLUSTERED ([OrganizationMetaDataId])
	)  
	INSERT INTO [Data].[TableVersion]([TableName],[Version]) VALUES('[Data].[OrganizationMetaData]', 0)
END

GO--
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[Data].[Voter]') AND type in (N'U'))
BEGIN
	CREATE TABLE [Data].[Voter](
		[VoterId] int IDENTITY(1,1) NOT NULL,
		[UserId] int  NOT NULL,
		[ContentInspectionId] int  NOT NULL,
		[LocationId] int  NOT NULL,
		[VoterName] varchar(150)  NOT NULL,
		[PostalCode] char(6)  NOT NULL,
		[FavoriteOrganizationId] int  NOT NULL,
		CONSTRAINT [PK_Voter] PRIMARY KEY CLUSTERED ([VoterId])
	)  
	INSERT INTO [Data].[TableVersion]([TableName],[Version]) VALUES('[Data].[Voter]', 0)
END

GO--
CREATE VIEW [Client].[Candidates] WITH SCHEMABINDING
AS
SELECT	
		C.[CandidateId],
		C.[ContentInspectionId],
		C.[CandidateName],
		C.[OrganizationId],
		CI.[IsArchived],
		CI.[IsBeingProposed],
		CI.[ProposedByUserId],
		CI.[ConfirmedByUserId],
		CI.[FalseInfoCount],
		CI.[TrueInfoCount],
		CI.[AdminInpsected],
		CI.[DateLastChecked],
		CI.[SourceUrl]
FROM	[Data].[Candidate] C
INNER JOIN [Data].[ContentInspection] CI
	ON C.[ContentInspectionId] = CI.[ContentInspectionId]


GO--
CREATE VIEW [Client].[Locations] WITH SCHEMABINDING
AS
SELECT	
		L.[LocationId],
		L.[ContentInspectionId],
		L.[LocationName],
		CI.[IsArchived],
		CI.[IsBeingProposed],
		CI.[ProposedByUserId],
		CI.[ConfirmedByUserId],
		CI.[FalseInfoCount],
		CI.[TrueInfoCount],
		CI.[AdminInpsected],
		CI.[DateLastChecked],
		CI.[SourceUrl]
FROM	[Data].[Location] L
INNER JOIN [Data].[ContentInspection] CI
	ON L.[ContentInspectionId] = CI.[ContentInspectionId]


GO--
CREATE VIEW [Client].[Organizations] WITH SCHEMABINDING
AS
SELECT	
		O.[OrganizationId],
		O.[ContentInspectionId],
		O.[OrganizationName],
		CI.[IsArchived],
		CI.[IsBeingProposed],
		CI.[ProposedByUserId],
		CI.[ConfirmedByUserId],
		CI.[FalseInfoCount],
		CI.[TrueInfoCount],
		CI.[AdminInpsected],
		CI.[DateLastChecked],
		CI.[SourceUrl]
FROM	[Data].[Organization] O
INNER JOIN [Data].[ContentInspection] CI
	ON O.[ContentInspectionId] = CI.[ContentInspectionId]


GO--
CREATE VIEW [Client].[Users] WITH SCHEMABINDING
AS
--Only return things you don't mind anyone seeing.
--Example: Protected email, usertoken, username...
SELECT [UserId]
      ,[DisplayName]
  FROM [Auth].[User]

GO--
CREATE FUNCTION [Auth].[GetUserIdByUserToken]
(
	@UserToken varchar(36)
)
RETURNS INT
AS
BEGIN
	DECLARE @UserId INT;
	SET @UserId = ISNULL(
		(
			SELECT [UserId] FROM [Auth].[User] WHERE [UserToken] = @UserToken 
		), 0)

	RETURN @UserId
END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[Permission_Delete]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[Permission_Delete]

GO--

CREATE PROCEDURE [Auth].[Permission_Delete] 
			@PermissionId int
AS --Generated--
BEGIN

	DELETE FROM	[Auth].[Permission]
	WHERE	[PermissionId] = @PermissionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[Permission_Exists]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[Permission_Exists]

GO--

CREATE PROCEDURE [Auth].[Permission_Exists]
			@PermissionId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	IF EXISTS(
		SELECT * FROM [Auth].[Permission]
		WHERE	[PermissionId] = @PermissionId
	) BEGIN
		SELECT CAST(1 as bit) as [Exists]
	END ELSE BEGIN
		SELECT CAST(0 as bit) as [Exists]
	END
END

GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[Permission_Insert]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[Permission_Insert]

GO--

CREATE PROCEDURE [Auth].[Permission_Insert]
			@PermissionName varchar(100),
			@Title varchar(150),
			@IsRead bit
AS --Generated--
BEGIN

	INSERT INTO [Auth].[Permission] (

			[PermissionName],
			[Title],
			[IsRead]
	) VALUES (

			@PermissionName,
			@Title,
			@IsRead
	)

	SELECT CAST(SCOPE_IDENTITY() AS int) AS [PermissionId]
END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[Permission_List]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[Permission_List]

GO--

CREATE PROCEDURE [Auth].[Permission_List] 
			@Title varchar(150) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
		[PermissionId] as [ListKey],
		[Title] AS [ListLabel]			
	FROM	[Auth].[Permission]
	WHERE	(@Title IS NULL OR [Title] LIKE '%' + @Title + '%')
	ORDER BY [ListLabel] ASC

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[Permission_Search]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[Permission_Search]

GO--

CREATE PROCEDURE [Auth].[Permission_Search] 
			@PermissionName varchar(100) = NULL,
			@Title varchar(150) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[PermissionId],
			[PermissionName],
			[Title],
			[IsRead]
	FROM	[Auth].[Permission]
	WHERE	(@PermissionName IS NULL OR [PermissionName] LIKE '%' + @PermissionName + '%')
			AND (@Title IS NULL OR [Title] LIKE '%' + @Title + '%')

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[Permission_SelectAll]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[Permission_SelectAll]

GO--

CREATE PROCEDURE [Auth].[Permission_SelectAll]
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[PermissionId],
			[PermissionName],
			[Title],
			[IsRead]
	FROM	[Auth].[Permission]

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[Permission_SelectBy_PermissionId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[Permission_SelectBy_PermissionId]

GO--

CREATE PROCEDURE [Auth].[Permission_SelectBy_PermissionId] 
			@PermissionId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[PermissionId],
			[PermissionName],
			[Title],
			[IsRead]
	FROM	[Auth].[Permission]
	WHERE	[PermissionId] = @PermissionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[Permission_SelectBy_UserId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[Permission_SelectBy_UserId]

GO--

CREATE PROCEDURE [Auth].[Permission_SelectBy_UserId] 
			@UserId int
AS 
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[PermissionId],
			[PermissionName],
			[Title],
			[IsRead]
	FROM	[Auth].[Permission]
	WHERE	
	[PermissionId] IN -- Anonymous / All users permissions
	(	SELECT	[PermissionId]
		FROM	[Auth].[RolePermission] RP
		INNER JOIN
				[Auth].[Role] R
				ON RP.[RoleId] = R.[RoleId]
		WHERE
			(R.[ApplyToAnon] = 1 OR (R.[ApplyToAllUsers] = 1 AND @UserId > 0))
			AND R.[IsActive] = 1
	) OR [PermissionId] IN -- Specifically assigned permissions
	(	SELECT	[PermissionId]
		FROM	[Auth].[RolePermission] RP
		INNER JOIN
				[Auth].[Role] R
				ON RP.[RoleId] = R.[RoleId]
		INNER JOIN 
				[Auth].[UserRole] UR
				ON R.[RoleId] = UR.[RoleId]
		WHERE
			UR.[UserId] = @UserId
			AND R.[IsActive] = 1
	)
		

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[Permission_Update]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[Permission_Update]

GO--

CREATE PROCEDURE [Auth].[Permission_Update] 
			@PermissionId int,
			@PermissionName varchar(100),
			@Title varchar(150),
			@IsRead bit
AS --Generated--
BEGIN

	UPDATE	[Auth].[Permission] SET 
			[PermissionName] = @PermissionName,
			[Title] = @Title,
			[IsRead] = @IsRead
	WHERE	[PermissionId] = @PermissionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[Role_Delete]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[Role_Delete]

GO--

CREATE PROCEDURE [Auth].[Role_Delete] 
			@RoleId int
AS --Generated--
BEGIN

	DELETE FROM	[Auth].[Role]
	WHERE	[RoleId] = @RoleId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[Role_Exists]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[Role_Exists]

GO--

CREATE PROCEDURE [Auth].[Role_Exists]
			@RoleId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	IF EXISTS(
		SELECT * FROM [Auth].[Role]
		WHERE	[RoleId] = @RoleId
	) BEGIN
		SELECT CAST(1 as bit) as [Exists]
	END ELSE BEGIN
		SELECT CAST(0 as bit) as [Exists]
	END
END

GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[Role_Insert]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[Role_Insert]

GO--

CREATE PROCEDURE [Auth].[Role_Insert]
			@Title varchar(50),
			@Description varchar(max),
			@IsActive bit,
			@ApplyToAnon bit,
			@ApplyToAllUsers bit,
			@PreventAddingUsers bit,
			@WINSID varchar(50) = NULL
AS --Generated--
BEGIN

	INSERT INTO [Auth].[Role] (

			[Title],
			[Description],
			[IsActive],
			[ApplyToAnon],
			[ApplyToAllUsers],
			[PreventAddingUsers],
			[WINSID]
	) VALUES (

			@Title,
			@Description,
			@IsActive,
			@ApplyToAnon,
			@ApplyToAllUsers,
			@PreventAddingUsers,
			@WINSID
	)

	SELECT CAST(SCOPE_IDENTITY() AS int) AS [RoleId]
END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[Role_List]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[Role_List]

GO--

CREATE PROCEDURE [Auth].[Role_List] 
			@Title varchar(50) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
		[RoleId] as [ListKey],
		[Title] AS [ListLabel]			
	FROM	[Auth].[Role]
	WHERE	(@Title IS NULL OR [Title] LIKE '%' + @Title + '%')
	ORDER BY [ListLabel] ASC

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[Role_Search]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[Role_Search]

GO--

CREATE PROCEDURE [Auth].[Role_Search] 
			@Title varchar(50) = NULL,
			@Description varchar(max) = NULL,
			@WINSID varchar(50) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[RoleId],
			[Title],
			[Description],
			[IsActive],
			[ApplyToAnon],
			[ApplyToAllUsers],
			[PreventAddingUsers],
			[WINSID]
	FROM	[Auth].[Role]
	WHERE	(@Title IS NULL OR [Title] LIKE '%' + @Title + '%')
			AND (@Description IS NULL OR [Description] LIKE '%' + @Description + '%')
			AND (@WINSID IS NULL OR [WINSID] LIKE '%' + @WINSID + '%')

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[Role_SelectAll]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[Role_SelectAll]

GO--

CREATE PROCEDURE [Auth].[Role_SelectAll]
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[RoleId],
			[Title],
			[Description],
			[IsActive],
			[ApplyToAnon],
			[ApplyToAllUsers],
			[PreventAddingUsers],
			[WINSID]
	FROM	[Auth].[Role]

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[Role_SelectBy_RoleId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[Role_SelectBy_RoleId]

GO--

CREATE PROCEDURE [Auth].[Role_SelectBy_RoleId] 
			@RoleId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[RoleId],
			[Title],
			[Description],
			[IsActive],
			[ApplyToAnon],
			[ApplyToAllUsers],
			[PreventAddingUsers],
			[WINSID]
	FROM	[Auth].[Role]
	WHERE	[RoleId] = @RoleId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[Role_Update]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[Role_Update]

GO--

CREATE PROCEDURE [Auth].[Role_Update] 
			@RoleId int,
			@Title varchar(50),
			@Description varchar(max),
			@IsActive bit,
			@ApplyToAnon bit,
			@ApplyToAllUsers bit,
			@PreventAddingUsers bit,
			@WINSID varchar(50) = NULL
AS --Generated--
BEGIN

	UPDATE	[Auth].[Role] SET 
			[Title] = @Title,
			[Description] = @Description,
			[IsActive] = @IsActive,
			[ApplyToAnon] = @ApplyToAnon,
			[ApplyToAllUsers] = @ApplyToAllUsers,
			[PreventAddingUsers] = @PreventAddingUsers,
			[WINSID] = @WINSID
	WHERE	[RoleId] = @RoleId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[RolePermission_Delete]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[RolePermission_Delete]

GO--

CREATE PROCEDURE [Auth].[RolePermission_Delete] 
			@RolePermissionId int
AS --Generated--
BEGIN

	DELETE FROM	[Auth].[RolePermission]
	WHERE	[RolePermissionId] = @RolePermissionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[RolePermission_Exists]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[RolePermission_Exists]

GO--

CREATE PROCEDURE [Auth].[RolePermission_Exists]
			@RolePermissionId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	IF EXISTS(
		SELECT * FROM [Auth].[RolePermission]
		WHERE	[RolePermissionId] = @RolePermissionId
	) BEGIN
		SELECT CAST(1 as bit) as [Exists]
	END ELSE BEGIN
		SELECT CAST(0 as bit) as [Exists]
	END
END

GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[RolePermission_Insert]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[RolePermission_Insert]

GO--

CREATE PROCEDURE [Auth].[RolePermission_Insert]
			@RoleId int,
			@PermissionId int
AS --Generated--
BEGIN

	INSERT INTO [Auth].[RolePermission] (

			[RoleId],
			[PermissionId]
	) VALUES (

			@RoleId,
			@PermissionId
	)

	SELECT CAST(SCOPE_IDENTITY() AS int) AS [RolePermissionId]
END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[RolePermission_SelectAll]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[RolePermission_SelectAll]

GO--

CREATE PROCEDURE [Auth].[RolePermission_SelectAll]
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[RolePermissionId],
			[RoleId],
			[PermissionId]
	FROM	[Auth].[RolePermission]

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[RolePermission_SelectBy_PermissionId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[RolePermission_SelectBy_PermissionId]

GO--

CREATE PROCEDURE [Auth].[RolePermission_SelectBy_PermissionId] 
			@PermissionId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[RolePermissionId],
			[RoleId],
			[PermissionId]
	FROM	[Auth].[RolePermission]
	WHERE	[PermissionId] = @PermissionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[RolePermission_SelectBy_RoleId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[RolePermission_SelectBy_RoleId]

GO--

CREATE PROCEDURE [Auth].[RolePermission_SelectBy_RoleId] 
			@RoleId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[RolePermissionId],
			[RoleId],
			[PermissionId]
	FROM	[Auth].[RolePermission]
	WHERE	[RoleId] = @RoleId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[RolePermission_SelectBy_RolePermissionId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[RolePermission_SelectBy_RolePermissionId]

GO--

CREATE PROCEDURE [Auth].[RolePermission_SelectBy_RolePermissionId] 
			@RolePermissionId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[RolePermissionId],
			[RoleId],
			[PermissionId]
	FROM	[Auth].[RolePermission]
	WHERE	[RolePermissionId] = @RolePermissionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[RolePermission_Update]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[RolePermission_Update]

GO--

CREATE PROCEDURE [Auth].[RolePermission_Update] 
			@RolePermissionId int,
			@RoleId int,
			@PermissionId int
AS --Generated--
BEGIN

	UPDATE	[Auth].[RolePermission] SET 
			[RoleId] = @RoleId,
			[PermissionId] = @PermissionId
	WHERE	[RolePermissionId] = @RolePermissionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[TableVersion_Delete]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[TableVersion_Delete]

GO--

CREATE PROCEDURE [Auth].[TableVersion_Delete] 
			@TableId int
AS --Generated--
BEGIN

	DELETE FROM	[Auth].[TableVersion]
	WHERE	[TableId] = @TableId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[TableVersion_Exists]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[TableVersion_Exists]

GO--

CREATE PROCEDURE [Auth].[TableVersion_Exists]
			@TableId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	IF EXISTS(
		SELECT * FROM [Auth].[TableVersion]
		WHERE	[TableId] = @TableId
	) BEGIN
		SELECT CAST(1 as bit) as [Exists]
	END ELSE BEGIN
		SELECT CAST(0 as bit) as [Exists]
	END
END

GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[TableVersion_Insert]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[TableVersion_Insert]

GO--

CREATE PROCEDURE [Auth].[TableVersion_Insert]
			@TableName varchar(150),
			@Version int
AS --Generated--
BEGIN

	INSERT INTO [Auth].[TableVersion] (

			[TableName],
			[Version]
	) VALUES (

			@TableName,
			@Version
	)

	SELECT CAST(SCOPE_IDENTITY() AS int) AS [TableId]
END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[TableVersion_Search]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[TableVersion_Search]

GO--

CREATE PROCEDURE [Auth].[TableVersion_Search] 
			@TableName varchar(150) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[TableId],
			[TableName],
			[Version]
	FROM	[Auth].[TableVersion]
	WHERE	(@TableName IS NULL OR [TableName] LIKE '%' + @TableName + '%')

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[TableVersion_SelectAll]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[TableVersion_SelectAll]

GO--

CREATE PROCEDURE [Auth].[TableVersion_SelectAll]
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[TableId],
			[TableName],
			[Version]
	FROM	[Auth].[TableVersion]

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[TableVersion_SelectBy_TableId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[TableVersion_SelectBy_TableId]

GO--

CREATE PROCEDURE [Auth].[TableVersion_SelectBy_TableId] 
			@TableId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[TableId],
			[TableName],
			[Version]
	FROM	[Auth].[TableVersion]
	WHERE	[TableId] = @TableId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[TableVersion_Update]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[TableVersion_Update]

GO--

CREATE PROCEDURE [Auth].[TableVersion_Update] 
			@TableId int,
			@TableName varchar(150),
			@Version int
AS --Generated--
BEGIN

	UPDATE	[Auth].[TableVersion] SET 
			[TableName] = @TableName,
			[Version] = @Version
	WHERE	[TableId] = @TableId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[User_Delete]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[User_Delete]

GO--

CREATE PROCEDURE [Auth].[User_Delete] 
			@UserId int
AS --Generated--
BEGIN

	DELETE FROM	[Auth].[User]
	WHERE	[UserId] = @UserId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[User_Exists]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[User_Exists]

GO--

CREATE PROCEDURE [Auth].[User_Exists]
			@UserId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	IF EXISTS(
		SELECT * FROM [Auth].[User]
		WHERE	[UserId] = @UserId
	) BEGIN
		SELECT CAST(1 as bit) as [Exists]
	END ELSE BEGIN
		SELECT CAST(0 as bit) as [Exists]
	END
END

GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[User_Insert]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[User_Insert]

GO--

CREATE PROCEDURE [Auth].[User_Insert]
			@UserName varchar(50),
			@Password binary(16),
			@DisplayName varchar(50),
			@Email varchar(100),
			@AuthToken uniqueidentifier,
			@UserToken uniqueidentifier,
			@FailedLogins int,
			@IsActive bit,
			@WINSID varchar(50) = NULL
AS --Generated--
BEGIN

	INSERT INTO [Auth].[User] (

			[UserName],
			[Password],
			[DisplayName],
			[Email],
			[AuthToken],
			[UserToken],
			[FailedLogins],
			[IsActive],
			[WINSID]
	) VALUES (

			@UserName,
			@Password,
			@DisplayName,
			@Email,
			@AuthToken,
			@UserToken,
			@FailedLogins,
			@IsActive,
			@WINSID
	)

	SELECT CAST(SCOPE_IDENTITY() AS int) AS [UserId]
END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[User_List]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[User_List]

GO--

CREATE PROCEDURE [Auth].[User_List] 
			@UserName varchar(50) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
		[UserId] as [ListKey],
		[UserName] AS [ListLabel]			
	FROM	[Auth].[User]
	WHERE	(@UserName IS NULL OR [UserName] LIKE '%' + @UserName + '%')
	ORDER BY [ListLabel] ASC

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[User_Search]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[User_Search]

GO--

CREATE PROCEDURE [Auth].[User_Search] 
			@UserName varchar(50) = NULL,
			@DisplayName varchar(50) = NULL,
			@Email varchar(100) = NULL,
			@WINSID varchar(50) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[UserId],
			[UserName],
			[Password],
			[DisplayName],
			[Email],
			[AuthToken],
			[UserToken],
			[FailedLogins],
			[IsActive],
			[WINSID]
	FROM	[Auth].[User]
	WHERE	(@UserName IS NULL OR [UserName] LIKE '%' + @UserName + '%')
			AND (@DisplayName IS NULL OR [DisplayName] LIKE '%' + @DisplayName + '%')
			AND (@Email IS NULL OR [Email] LIKE '%' + @Email + '%')
			AND (@WINSID IS NULL OR [WINSID] LIKE '%' + @WINSID + '%')

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[User_SelectAll]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[User_SelectAll]

GO--

CREATE PROCEDURE [Auth].[User_SelectAll]
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[UserId],
			[UserName],
			[Password],
			[DisplayName],
			[Email],
			[AuthToken],
			[UserToken],
			[FailedLogins],
			[IsActive],
			[WINSID]
	FROM	[Auth].[User]

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[User_SelectBy_Email]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[User_SelectBy_Email]

GO--

CREATE PROCEDURE [Auth].[User_SelectBy_Email] 
			@Email varchar(100)
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[UserId],
			[UserName],
			[Password],
			[DisplayName],
			[Email],
			[AuthToken],
			[UserToken],
			[FailedLogins],
			[IsActive],
			[WINSID]
	FROM	[Auth].[User]
	WHERE	[Email] = @Email

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[User_SelectBy_UserId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[User_SelectBy_UserId]

GO--

CREATE PROCEDURE [Auth].[User_SelectBy_UserId] 
			@UserId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[UserId],
			[UserName],
			[Password],
			[DisplayName],
			[Email],
			[AuthToken],
			[UserToken],
			[FailedLogins],
			[IsActive],
			[WINSID]
	FROM	[Auth].[User]
	WHERE	[UserId] = @UserId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[User_SelectBy_UserName]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[User_SelectBy_UserName]

GO--

CREATE PROCEDURE [Auth].[User_SelectBy_UserName] 
			@UserName varchar(50)
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[UserId],
			[UserName],
			[Password],
			[DisplayName],
			[Email],
			[AuthToken],
			[UserToken],
			[FailedLogins],
			[IsActive],
			[WINSID]
	FROM	[Auth].[User]
	WHERE	[UserName] = @UserName

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[User_SelectBy_UserToken]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[User_SelectBy_UserToken]

GO--

CREATE PROCEDURE [Auth].[User_SelectBy_UserToken] 
			@UserToken uniqueidentifier
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[UserId],
			[UserName],
			[Password],
			[DisplayName],
			[Email],
			[AuthToken],
			[UserToken],
			[FailedLogins],
			[IsActive],
			[WINSID]
	FROM	[Auth].[User]
	WHERE	[UserToken] = @UserToken

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[User_SelectBy_WINSID]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[User_SelectBy_WINSID]

GO--

CREATE PROCEDURE [Auth].[User_SelectBy_WINSID] 
			@WINSID varchar(50)
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[UserId],
			[UserName],
			[Password],
			[DisplayName],
			[Email],
			[AuthToken],
			[UserToken],
			[FailedLogins],
			[IsActive],
			[WINSID]
	FROM	[Auth].[User]
	WHERE	[WINSID] = @WINSID

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[User_Update]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[User_Update]

GO--

CREATE PROCEDURE [Auth].[User_Update] 
			@UserId int,
			@UserName varchar(50),
			@Password binary(16),
			@DisplayName varchar(50),
			@Email varchar(100),
			@AuthToken uniqueidentifier,
			@UserToken uniqueidentifier,
			@FailedLogins int,
			@IsActive bit,
			@WINSID varchar(50) = NULL
AS --Generated--
BEGIN

	UPDATE	[Auth].[User] SET 
			[UserName] = @UserName,
			[Password] = @Password,
			[DisplayName] = @DisplayName,
			[Email] = @Email,
			[AuthToken] = @AuthToken,
			[UserToken] = @UserToken,
			[FailedLogins] = @FailedLogins,
			[IsActive] = @IsActive,
			[WINSID] = @WINSID
	WHERE	[UserId] = @UserId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[UserRole_Delete]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[UserRole_Delete]

GO--

CREATE PROCEDURE [Auth].[UserRole_Delete] 
			@UserRoleId int
AS --Generated--
BEGIN

	DELETE FROM	[Auth].[UserRole]
	WHERE	[UserRoleId] = @UserRoleId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[UserRole_Exists]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[UserRole_Exists]

GO--

CREATE PROCEDURE [Auth].[UserRole_Exists]
			@UserRoleId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	IF EXISTS(
		SELECT * FROM [Auth].[UserRole]
		WHERE	[UserRoleId] = @UserRoleId
	) BEGIN
		SELECT CAST(1 as bit) as [Exists]
	END ELSE BEGIN
		SELECT CAST(0 as bit) as [Exists]
	END
END

GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[UserRole_Insert]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[UserRole_Insert]

GO--

CREATE PROCEDURE [Auth].[UserRole_Insert]
			@UserId int,
			@RoleId int
AS --Generated--
BEGIN

	INSERT INTO [Auth].[UserRole] (

			[UserId],
			[RoleId]
	) VALUES (

			@UserId,
			@RoleId
	)

	SELECT CAST(SCOPE_IDENTITY() AS int) AS [UserRoleId]
END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[UserRole_SelectAll]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[UserRole_SelectAll]

GO--

CREATE PROCEDURE [Auth].[UserRole_SelectAll]
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[UserRoleId],
			[UserId],
			[RoleId]
	FROM	[Auth].[UserRole]

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[UserRole_SelectBy_RoleId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[UserRole_SelectBy_RoleId]

GO--

CREATE PROCEDURE [Auth].[UserRole_SelectBy_RoleId] 
			@RoleId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[UserRoleId],
			[UserId],
			[RoleId]
	FROM	[Auth].[UserRole]
	WHERE	[RoleId] = @RoleId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[UserRole_SelectBy_UserId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[UserRole_SelectBy_UserId]

GO--

CREATE PROCEDURE [Auth].[UserRole_SelectBy_UserId] 
			@UserId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[UserRoleId],
			[UserId],
			[RoleId]
	FROM	[Auth].[UserRole]
	WHERE	[UserId] = @UserId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[UserRole_SelectBy_UserRoleId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[UserRole_SelectBy_UserRoleId]

GO--

CREATE PROCEDURE [Auth].[UserRole_SelectBy_UserRoleId] 
			@UserRoleId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[UserRoleId],
			[UserId],
			[RoleId]
	FROM	[Auth].[UserRole]
	WHERE	[UserRoleId] = @UserRoleId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[UserRole_Update]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[UserRole_Update]

GO--

CREATE PROCEDURE [Auth].[UserRole_Update] 
			@UserRoleId int,
			@UserId int,
			@RoleId int
AS --Generated--
BEGIN

	UPDATE	[Auth].[UserRole] SET 
			[UserId] = @UserId,
			[RoleId] = @RoleId
	WHERE	[UserRoleId] = @UserRoleId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Candidates_Exists]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Candidates_Exists]

GO--

CREATE PROCEDURE [Client].[Candidates_Exists]
			@CandidateId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	IF EXISTS(
		SELECT * FROM [Client].[Candidates]
		WHERE	[CandidateId] = @CandidateId
	) BEGIN
		SELECT CAST(1 as bit) as [Exists]
	END ELSE BEGIN
		SELECT CAST(0 as bit) as [Exists]
	END
END

GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Candidates_Search]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Candidates_Search]

GO--

CREATE PROCEDURE [Client].[Candidates_Search] 
			@CandidateName varchar(150) = NULL,
			@SourceUrl varchar(250) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[CandidateId],
			[ContentInspectionId],
			[CandidateName],
			[OrganizationId],
			[IsArchived],
			[IsBeingProposed],
			[ProposedByUserId],
			[ConfirmedByUserId],
			[FalseInfoCount],
			[TrueInfoCount],
			[AdminInpsected],
			[DateLastChecked],
			[SourceUrl]
	FROM	[Client].[Candidates]
	WHERE	(@CandidateName IS NULL OR [CandidateName] LIKE '%' + @CandidateName + '%')
			AND (@SourceUrl IS NULL OR [SourceUrl] LIKE '%' + @SourceUrl + '%')

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Candidates_SelectAll]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Candidates_SelectAll]

GO--

CREATE PROCEDURE [Client].[Candidates_SelectAll]
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[CandidateId],
			[ContentInspectionId],
			[CandidateName],
			[OrganizationId],
			[IsArchived],
			[IsBeingProposed],
			[ProposedByUserId],
			[ConfirmedByUserId],
			[FalseInfoCount],
			[TrueInfoCount],
			[AdminInpsected],
			[DateLastChecked],
			[SourceUrl]
	FROM	[Client].[Candidates]

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Candidates_SelectBy_CandidateId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Candidates_SelectBy_CandidateId]

GO--

CREATE PROCEDURE [Client].[Candidates_SelectBy_CandidateId] 
			@CandidateId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[CandidateId],
			[ContentInspectionId],
			[CandidateName],
			[OrganizationId],
			[IsArchived],
			[IsBeingProposed],
			[ProposedByUserId],
			[ConfirmedByUserId],
			[FalseInfoCount],
			[TrueInfoCount],
			[AdminInpsected],
			[DateLastChecked],
			[SourceUrl]
	FROM	[Client].[Candidates]
	WHERE	[CandidateId] = @CandidateId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Candidates_SelectBy_ConfirmedByUserId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Candidates_SelectBy_ConfirmedByUserId]

GO--

CREATE PROCEDURE [Client].[Candidates_SelectBy_ConfirmedByUserId] 
			@ConfirmedByUserId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[CandidateId],
			[ContentInspectionId],
			[CandidateName],
			[OrganizationId],
			[IsArchived],
			[IsBeingProposed],
			[ProposedByUserId],
			[ConfirmedByUserId],
			[FalseInfoCount],
			[TrueInfoCount],
			[AdminInpsected],
			[DateLastChecked],
			[SourceUrl]
	FROM	[Client].[Candidates]
	WHERE	[ConfirmedByUserId] = @ConfirmedByUserId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Candidates_SelectBy_ContentInspectionId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Candidates_SelectBy_ContentInspectionId]

GO--

CREATE PROCEDURE [Client].[Candidates_SelectBy_ContentInspectionId] 
			@ContentInspectionId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[CandidateId],
			[ContentInspectionId],
			[CandidateName],
			[OrganizationId],
			[IsArchived],
			[IsBeingProposed],
			[ProposedByUserId],
			[ConfirmedByUserId],
			[FalseInfoCount],
			[TrueInfoCount],
			[AdminInpsected],
			[DateLastChecked],
			[SourceUrl]
	FROM	[Client].[Candidates]
	WHERE	[ContentInspectionId] = @ContentInspectionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Candidates_SelectBy_OrganizationId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Candidates_SelectBy_OrganizationId]

GO--

CREATE PROCEDURE [Client].[Candidates_SelectBy_OrganizationId] 
			@OrganizationId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[CandidateId],
			[ContentInspectionId],
			[CandidateName],
			[OrganizationId],
			[IsArchived],
			[IsBeingProposed],
			[ProposedByUserId],
			[ConfirmedByUserId],
			[FalseInfoCount],
			[TrueInfoCount],
			[AdminInpsected],
			[DateLastChecked],
			[SourceUrl]
	FROM	[Client].[Candidates]
	WHERE	[OrganizationId] = @OrganizationId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Candidates_SelectBy_ProposedByUserId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Candidates_SelectBy_ProposedByUserId]

GO--

CREATE PROCEDURE [Client].[Candidates_SelectBy_ProposedByUserId] 
			@ProposedByUserId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[CandidateId],
			[ContentInspectionId],
			[CandidateName],
			[OrganizationId],
			[IsArchived],
			[IsBeingProposed],
			[ProposedByUserId],
			[ConfirmedByUserId],
			[FalseInfoCount],
			[TrueInfoCount],
			[AdminInpsected],
			[DateLastChecked],
			[SourceUrl]
	FROM	[Client].[Candidates]
	WHERE	[ProposedByUserId] = @ProposedByUserId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Locations_Search]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Locations_Search]

GO--

CREATE PROCEDURE [Client].[Locations_Search] 
			@LocationName varchar(150) = NULL,
			@SourceUrl varchar(250) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[LocationId],
			[ContentInspectionId],
			[LocationName],
			[IsArchived],
			[IsBeingProposed],
			[ProposedByUserId],
			[ConfirmedByUserId],
			[FalseInfoCount],
			[TrueInfoCount],
			[AdminInpsected],
			[DateLastChecked],
			[SourceUrl]
	FROM	[Client].[Locations]
	WHERE	(@LocationName IS NULL OR [LocationName] LIKE '%' + @LocationName + '%')
			AND (@SourceUrl IS NULL OR [SourceUrl] LIKE '%' + @SourceUrl + '%')

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Locations_SelectAll]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Locations_SelectAll]

GO--

CREATE PROCEDURE [Client].[Locations_SelectAll]
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[LocationId],
			[ContentInspectionId],
			[LocationName],
			[IsArchived],
			[IsBeingProposed],
			[ProposedByUserId],
			[ConfirmedByUserId],
			[FalseInfoCount],
			[TrueInfoCount],
			[AdminInpsected],
			[DateLastChecked],
			[SourceUrl]
	FROM	[Client].[Locations]

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Locations_SelectBy_ConfirmedByUserId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Locations_SelectBy_ConfirmedByUserId]

GO--

CREATE PROCEDURE [Client].[Locations_SelectBy_ConfirmedByUserId] 
			@ConfirmedByUserId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[LocationId],
			[ContentInspectionId],
			[LocationName],
			[IsArchived],
			[IsBeingProposed],
			[ProposedByUserId],
			[ConfirmedByUserId],
			[FalseInfoCount],
			[TrueInfoCount],
			[AdminInpsected],
			[DateLastChecked],
			[SourceUrl]
	FROM	[Client].[Locations]
	WHERE	[ConfirmedByUserId] = @ConfirmedByUserId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Locations_SelectBy_ContentInspectionId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Locations_SelectBy_ContentInspectionId]

GO--

CREATE PROCEDURE [Client].[Locations_SelectBy_ContentInspectionId] 
			@ContentInspectionId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[LocationId],
			[ContentInspectionId],
			[LocationName],
			[IsArchived],
			[IsBeingProposed],
			[ProposedByUserId],
			[ConfirmedByUserId],
			[FalseInfoCount],
			[TrueInfoCount],
			[AdminInpsected],
			[DateLastChecked],
			[SourceUrl]
	FROM	[Client].[Locations]
	WHERE	[ContentInspectionId] = @ContentInspectionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Locations_SelectBy_LocationId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Locations_SelectBy_LocationId]

GO--

CREATE PROCEDURE [Client].[Locations_SelectBy_LocationId] 
			@LocationId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[LocationId],
			[ContentInspectionId],
			[LocationName],
			[IsArchived],
			[IsBeingProposed],
			[ProposedByUserId],
			[ConfirmedByUserId],
			[FalseInfoCount],
			[TrueInfoCount],
			[AdminInpsected],
			[DateLastChecked],
			[SourceUrl]
	FROM	[Client].[Locations]
	WHERE	[LocationId] = @LocationId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Locations_SelectBy_ProposedByUserId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Locations_SelectBy_ProposedByUserId]

GO--

CREATE PROCEDURE [Client].[Locations_SelectBy_ProposedByUserId] 
			@ProposedByUserId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[LocationId],
			[ContentInspectionId],
			[LocationName],
			[IsArchived],
			[IsBeingProposed],
			[ProposedByUserId],
			[ConfirmedByUserId],
			[FalseInfoCount],
			[TrueInfoCount],
			[AdminInpsected],
			[DateLastChecked],
			[SourceUrl]
	FROM	[Client].[Locations]
	WHERE	[ProposedByUserId] = @ProposedByUserId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Organizations_Exists]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Organizations_Exists]

GO--

CREATE PROCEDURE [Client].[Organizations_Exists]
			@OrganizationId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	IF EXISTS(
		SELECT * FROM [Client].[Organizations]
		WHERE	[OrganizationId] = @OrganizationId
	) BEGIN
		SELECT CAST(1 as bit) as [Exists]
	END ELSE BEGIN
		SELECT CAST(0 as bit) as [Exists]
	END
END

GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Organizations_Search]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Organizations_Search]

GO--

CREATE PROCEDURE [Client].[Organizations_Search] 
			@OrganizationName varchar(250) = NULL,
			@SourceUrl varchar(250) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[OrganizationId],
			[ContentInspectionId],
			[OrganizationName],
			[IsArchived],
			[IsBeingProposed],
			[ProposedByUserId],
			[ConfirmedByUserId],
			[FalseInfoCount],
			[TrueInfoCount],
			[AdminInpsected],
			[DateLastChecked],
			[SourceUrl]
	FROM	[Client].[Organizations]
	WHERE	(@OrganizationName IS NULL OR [OrganizationName] LIKE '%' + @OrganizationName + '%')
			AND (@SourceUrl IS NULL OR [SourceUrl] LIKE '%' + @SourceUrl + '%')

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Organizations_SelectAll]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Organizations_SelectAll]

GO--

CREATE PROCEDURE [Client].[Organizations_SelectAll]
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[OrganizationId],
			[ContentInspectionId],
			[OrganizationName],
			[IsArchived],
			[IsBeingProposed],
			[ProposedByUserId],
			[ConfirmedByUserId],
			[FalseInfoCount],
			[TrueInfoCount],
			[AdminInpsected],
			[DateLastChecked],
			[SourceUrl]
	FROM	[Client].[Organizations]

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Organizations_SelectBy_ConfirmedByUserId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Organizations_SelectBy_ConfirmedByUserId]

GO--

CREATE PROCEDURE [Client].[Organizations_SelectBy_ConfirmedByUserId] 
			@ConfirmedByUserId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[OrganizationId],
			[ContentInspectionId],
			[OrganizationName],
			[IsArchived],
			[IsBeingProposed],
			[ProposedByUserId],
			[ConfirmedByUserId],
			[FalseInfoCount],
			[TrueInfoCount],
			[AdminInpsected],
			[DateLastChecked],
			[SourceUrl]
	FROM	[Client].[Organizations]
	WHERE	[ConfirmedByUserId] = @ConfirmedByUserId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Organizations_SelectBy_ContentInspectionId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Organizations_SelectBy_ContentInspectionId]

GO--

CREATE PROCEDURE [Client].[Organizations_SelectBy_ContentInspectionId] 
			@ContentInspectionId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[OrganizationId],
			[ContentInspectionId],
			[OrganizationName],
			[IsArchived],
			[IsBeingProposed],
			[ProposedByUserId],
			[ConfirmedByUserId],
			[FalseInfoCount],
			[TrueInfoCount],
			[AdminInpsected],
			[DateLastChecked],
			[SourceUrl]
	FROM	[Client].[Organizations]
	WHERE	[ContentInspectionId] = @ContentInspectionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Organizations_SelectBy_OrganizationId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Organizations_SelectBy_OrganizationId]

GO--

CREATE PROCEDURE [Client].[Organizations_SelectBy_OrganizationId] 
			@OrganizationId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[OrganizationId],
			[ContentInspectionId],
			[OrganizationName],
			[IsArchived],
			[IsBeingProposed],
			[ProposedByUserId],
			[ConfirmedByUserId],
			[FalseInfoCount],
			[TrueInfoCount],
			[AdminInpsected],
			[DateLastChecked],
			[SourceUrl]
	FROM	[Client].[Organizations]
	WHERE	[OrganizationId] = @OrganizationId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Organizations_SelectBy_ProposedByUserId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Organizations_SelectBy_ProposedByUserId]

GO--

CREATE PROCEDURE [Client].[Organizations_SelectBy_ProposedByUserId] 
			@ProposedByUserId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[OrganizationId],
			[ContentInspectionId],
			[OrganizationName],
			[IsArchived],
			[IsBeingProposed],
			[ProposedByUserId],
			[ConfirmedByUserId],
			[FalseInfoCount],
			[TrueInfoCount],
			[AdminInpsected],
			[DateLastChecked],
			[SourceUrl]
	FROM	[Client].[Organizations]
	WHERE	[ProposedByUserId] = @ProposedByUserId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[TableVersion_Delete]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[TableVersion_Delete]

GO--

CREATE PROCEDURE [Client].[TableVersion_Delete] 
			@TableId int
AS --Generated--
BEGIN

	DELETE FROM	[Client].[TableVersion]
	WHERE	[TableId] = @TableId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[TableVersion_Exists]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[TableVersion_Exists]

GO--

CREATE PROCEDURE [Client].[TableVersion_Exists]
			@TableId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	IF EXISTS(
		SELECT * FROM [Client].[TableVersion]
		WHERE	[TableId] = @TableId
	) BEGIN
		SELECT CAST(1 as bit) as [Exists]
	END ELSE BEGIN
		SELECT CAST(0 as bit) as [Exists]
	END
END

GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[TableVersion_Insert]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[TableVersion_Insert]

GO--

CREATE PROCEDURE [Client].[TableVersion_Insert]
			@TableName varchar(150),
			@Version int
AS --Generated--
BEGIN

	INSERT INTO [Client].[TableVersion] (

			[TableName],
			[Version]
	) VALUES (

			@TableName,
			@Version
	)

	SELECT CAST(SCOPE_IDENTITY() AS int) AS [TableId]
END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[TableVersion_Search]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[TableVersion_Search]

GO--

CREATE PROCEDURE [Client].[TableVersion_Search] 
			@TableName varchar(150) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[TableId],
			[TableName],
			[Version]
	FROM	[Client].[TableVersion]
	WHERE	(@TableName IS NULL OR [TableName] LIKE '%' + @TableName + '%')

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[TableVersion_SelectAll]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[TableVersion_SelectAll]

GO--

CREATE PROCEDURE [Client].[TableVersion_SelectAll]
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[TableId],
			[TableName],
			[Version]
	FROM	[Client].[TableVersion]

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[TableVersion_SelectBy_TableId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[TableVersion_SelectBy_TableId]

GO--

CREATE PROCEDURE [Client].[TableVersion_SelectBy_TableId] 
			@TableId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[TableId],
			[TableName],
			[Version]
	FROM	[Client].[TableVersion]
	WHERE	[TableId] = @TableId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[TableVersion_Update]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[TableVersion_Update]

GO--

CREATE PROCEDURE [Client].[TableVersion_Update] 
			@TableId int,
			@TableName varchar(150),
			@Version int
AS --Generated--
BEGIN

	UPDATE	[Client].[TableVersion] SET 
			[TableName] = @TableName,
			[Version] = @Version
	WHERE	[TableId] = @TableId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Users_Exists]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Users_Exists]

GO--

CREATE PROCEDURE [Client].[Users_Exists]
			@UserId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	IF EXISTS(
		SELECT * FROM [Client].[Users]
		WHERE	[UserId] = @UserId
	) BEGIN
		SELECT CAST(1 as bit) as [Exists]
	END ELSE BEGIN
		SELECT CAST(0 as bit) as [Exists]
	END
END

GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Users_Search]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Users_Search]

GO--

CREATE PROCEDURE [Client].[Users_Search] 
			@DisplayName varchar(50) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[UserId],
			[DisplayName]
	FROM	[Client].[Users]
	WHERE	(@DisplayName IS NULL OR [DisplayName] LIKE '%' + @DisplayName + '%')

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Users_SelectAll]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Users_SelectAll]

GO--

CREATE PROCEDURE [Client].[Users_SelectAll]
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[UserId],
			[DisplayName]
	FROM	[Client].[Users]

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Users_SelectByUser_Current]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Users_SelectByUser_Current]

GO--

CREATE PROCEDURE [Client].[Users_SelectByUser_Current]
	@AuthUserId int
AS
BEGIN
	SET NOCOUNT ON;

	SELECT	*
	FROM	[Client].[Users]
	WHERE	[UserId] = @AuthUserId

END
GO--
IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Users_SelectByUser_UpdateProfile]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Users_SelectByUser_UpdateProfile]

GO--

CREATE PROCEDURE [Client].[Users_SelectByUser_UpdateProfile]
	@AuthUserId int,
	@DisplayName varchar(50),
	@Email varchar(100)
AS
BEGIN
	SET NOCOUNT ON;

	if( LEN(@DisplayName) <= 1 ) SET @DisplayName = 'My Name Is Invaid';


	UPDATE [Auth].[User] SET
	[DisplayName] = @DisplayName,
	[Email] = @Email
	WHERE	[UserId] = @AuthUserId


	SELECT	*
	FROM	[Client].[Users]
	WHERE	[UserId] = @AuthUserId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Candidate_Delete]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Candidate_Delete]

GO--

CREATE PROCEDURE [Data].[Candidate_Delete] 
			@CandidateId int
AS --Generated--
BEGIN

	DELETE FROM	[Data].[Candidate]
	WHERE	[CandidateId] = @CandidateId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Candidate_Exists]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Candidate_Exists]

GO--

CREATE PROCEDURE [Data].[Candidate_Exists]
			@CandidateId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	IF EXISTS(
		SELECT * FROM [Data].[Candidate]
		WHERE	[CandidateId] = @CandidateId
	) BEGIN
		SELECT CAST(1 as bit) as [Exists]
	END ELSE BEGIN
		SELECT CAST(0 as bit) as [Exists]
	END
END

GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Candidate_Insert]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Candidate_Insert]

GO--

CREATE PROCEDURE [Data].[Candidate_Insert]
			@UserId int,
			@ContentInspectionId int,
			@LocationId int,
			@OrganizationId int,
			@CandidateName varchar(150)
AS --Generated--
BEGIN

	INSERT INTO [Data].[Candidate] (

			[UserId],
			[ContentInspectionId],
			[LocationId],
			[OrganizationId],
			[CandidateName]
	) VALUES (

			@UserId,
			@ContentInspectionId,
			@LocationId,
			@OrganizationId,
			@CandidateName
	)

	SELECT CAST(SCOPE_IDENTITY() AS int) AS [CandidateId]
END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Candidate_List]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Candidate_List]

GO--

CREATE PROCEDURE [Data].[Candidate_List] 
			@CandidateName varchar(150) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
		[CandidateId] as [ListKey],
		[CandidateName] AS [ListLabel]			
	FROM	[Data].[Candidate]
	WHERE	(@CandidateName IS NULL OR [CandidateName] LIKE '%' + @CandidateName + '%')
	ORDER BY [ListLabel] ASC

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Candidate_Search]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Candidate_Search]

GO--

CREATE PROCEDURE [Data].[Candidate_Search] 
			@CandidateName varchar(150) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[CandidateId],
			[UserId],
			[ContentInspectionId],
			[LocationId],
			[OrganizationId],
			[CandidateName]
	FROM	[Data].[Candidate]
	WHERE	(@CandidateName IS NULL OR [CandidateName] LIKE '%' + @CandidateName + '%')

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Candidate_SelectAll]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Candidate_SelectAll]

GO--

CREATE PROCEDURE [Data].[Candidate_SelectAll]
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[CandidateId],
			[UserId],
			[ContentInspectionId],
			[LocationId],
			[OrganizationId],
			[CandidateName]
	FROM	[Data].[Candidate]

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Candidate_SelectBy_CandidateId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Candidate_SelectBy_CandidateId]

GO--

CREATE PROCEDURE [Data].[Candidate_SelectBy_CandidateId] 
			@CandidateId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[CandidateId],
			[UserId],
			[ContentInspectionId],
			[LocationId],
			[OrganizationId],
			[CandidateName]
	FROM	[Data].[Candidate]
	WHERE	[CandidateId] = @CandidateId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Candidate_SelectBy_ContentInspectionId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Candidate_SelectBy_ContentInspectionId]

GO--

CREATE PROCEDURE [Data].[Candidate_SelectBy_ContentInspectionId] 
			@ContentInspectionId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[CandidateId],
			[UserId],
			[ContentInspectionId],
			[LocationId],
			[OrganizationId],
			[CandidateName]
	FROM	[Data].[Candidate]
	WHERE	[ContentInspectionId] = @ContentInspectionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Candidate_SelectBy_LocationId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Candidate_SelectBy_LocationId]

GO--

CREATE PROCEDURE [Data].[Candidate_SelectBy_LocationId] 
			@LocationId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[CandidateId],
			[UserId],
			[ContentInspectionId],
			[LocationId],
			[OrganizationId],
			[CandidateName]
	FROM	[Data].[Candidate]
	WHERE	[LocationId] = @LocationId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Candidate_SelectBy_OrganizationId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Candidate_SelectBy_OrganizationId]

GO--

CREATE PROCEDURE [Data].[Candidate_SelectBy_OrganizationId] 
			@OrganizationId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[CandidateId],
			[UserId],
			[ContentInspectionId],
			[LocationId],
			[OrganizationId],
			[CandidateName]
	FROM	[Data].[Candidate]
	WHERE	[OrganizationId] = @OrganizationId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Candidate_SelectBy_UserId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Candidate_SelectBy_UserId]

GO--

CREATE PROCEDURE [Data].[Candidate_SelectBy_UserId] 
			@UserId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[CandidateId],
			[UserId],
			[ContentInspectionId],
			[LocationId],
			[OrganizationId],
			[CandidateName]
	FROM	[Data].[Candidate]
	WHERE	[UserId] = @UserId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Candidate_Update]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Candidate_Update]

GO--

CREATE PROCEDURE [Data].[Candidate_Update] 
			@CandidateId int,
			@UserId int,
			@ContentInspectionId int,
			@LocationId int,
			@OrganizationId int,
			@CandidateName varchar(150)
AS --Generated--
BEGIN

	UPDATE	[Data].[Candidate] SET 
			[UserId] = @UserId,
			[ContentInspectionId] = @ContentInspectionId,
			[LocationId] = @LocationId,
			[OrganizationId] = @OrganizationId,
			[CandidateName] = @CandidateName
	WHERE	[CandidateId] = @CandidateId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[CandidateMetaData_Delete]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[CandidateMetaData_Delete]

GO--

CREATE PROCEDURE [Data].[CandidateMetaData_Delete] 
			@CandidateMetaDataId int
AS --Generated--
BEGIN

	DELETE FROM	[Data].[CandidateMetaData]
	WHERE	[CandidateMetaDataId] = @CandidateMetaDataId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[CandidateMetaData_Exists]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[CandidateMetaData_Exists]

GO--

CREATE PROCEDURE [Data].[CandidateMetaData_Exists]
			@CandidateMetaDataId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	IF EXISTS(
		SELECT * FROM [Data].[CandidateMetaData]
		WHERE	[CandidateMetaDataId] = @CandidateMetaDataId
	) BEGIN
		SELECT CAST(1 as bit) as [Exists]
	END ELSE BEGIN
		SELECT CAST(0 as bit) as [Exists]
	END
END

GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[CandidateMetaData_Insert]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[CandidateMetaData_Insert]

GO--

CREATE PROCEDURE [Data].[CandidateMetaData_Insert]
			@ContentInspectionId int,
			@CandidateId int,
			@MetaDataId int,
			@MetaDataValue varchar(max)
AS --Generated--
BEGIN

	INSERT INTO [Data].[CandidateMetaData] (

			[ContentInspectionId],
			[CandidateId],
			[MetaDataId],
			[MetaDataValue]
	) VALUES (

			@ContentInspectionId,
			@CandidateId,
			@MetaDataId,
			@MetaDataValue
	)

	SELECT CAST(SCOPE_IDENTITY() AS int) AS [CandidateMetaDataId]
END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[CandidateMetaData_Search]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[CandidateMetaData_Search]

GO--

CREATE PROCEDURE [Data].[CandidateMetaData_Search] 
			@MetaDataValue varchar(max) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[CandidateMetaDataId],
			[ContentInspectionId],
			[CandidateId],
			[MetaDataId],
			[MetaDataValue]
	FROM	[Data].[CandidateMetaData]
	WHERE	(@MetaDataValue IS NULL OR [MetaDataValue] LIKE '%' + @MetaDataValue + '%')

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[CandidateMetaData_SelectAll]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[CandidateMetaData_SelectAll]

GO--

CREATE PROCEDURE [Data].[CandidateMetaData_SelectAll]
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[CandidateMetaDataId],
			[ContentInspectionId],
			[CandidateId],
			[MetaDataId],
			[MetaDataValue]
	FROM	[Data].[CandidateMetaData]

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[CandidateMetaData_SelectBy_CandidateId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[CandidateMetaData_SelectBy_CandidateId]

GO--

CREATE PROCEDURE [Data].[CandidateMetaData_SelectBy_CandidateId] 
			@CandidateId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[CandidateMetaDataId],
			[ContentInspectionId],
			[CandidateId],
			[MetaDataId],
			[MetaDataValue]
	FROM	[Data].[CandidateMetaData]
	WHERE	[CandidateId] = @CandidateId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[CandidateMetaData_SelectBy_CandidateMetaDataId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[CandidateMetaData_SelectBy_CandidateMetaDataId]

GO--

CREATE PROCEDURE [Data].[CandidateMetaData_SelectBy_CandidateMetaDataId] 
			@CandidateMetaDataId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[CandidateMetaDataId],
			[ContentInspectionId],
			[CandidateId],
			[MetaDataId],
			[MetaDataValue]
	FROM	[Data].[CandidateMetaData]
	WHERE	[CandidateMetaDataId] = @CandidateMetaDataId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[CandidateMetaData_SelectBy_ContentInspectionId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[CandidateMetaData_SelectBy_ContentInspectionId]

GO--

CREATE PROCEDURE [Data].[CandidateMetaData_SelectBy_ContentInspectionId] 
			@ContentInspectionId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[CandidateMetaDataId],
			[ContentInspectionId],
			[CandidateId],
			[MetaDataId],
			[MetaDataValue]
	FROM	[Data].[CandidateMetaData]
	WHERE	[ContentInspectionId] = @ContentInspectionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[CandidateMetaData_SelectBy_MetaDataId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[CandidateMetaData_SelectBy_MetaDataId]

GO--

CREATE PROCEDURE [Data].[CandidateMetaData_SelectBy_MetaDataId] 
			@MetaDataId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[CandidateMetaDataId],
			[ContentInspectionId],
			[CandidateId],
			[MetaDataId],
			[MetaDataValue]
	FROM	[Data].[CandidateMetaData]
	WHERE	[MetaDataId] = @MetaDataId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[CandidateMetaData_Update]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[CandidateMetaData_Update]

GO--

CREATE PROCEDURE [Data].[CandidateMetaData_Update] 
			@CandidateMetaDataId int,
			@ContentInspectionId int,
			@CandidateId int,
			@MetaDataId int,
			@MetaDataValue varchar(max)
AS --Generated--
BEGIN

	UPDATE	[Data].[CandidateMetaData] SET 
			[ContentInspectionId] = @ContentInspectionId,
			[CandidateId] = @CandidateId,
			[MetaDataId] = @MetaDataId,
			[MetaDataValue] = @MetaDataValue
	WHERE	[CandidateMetaDataId] = @CandidateMetaDataId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ContentInspection_Delete]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ContentInspection_Delete]

GO--

CREATE PROCEDURE [Data].[ContentInspection_Delete] 
			@ContentInspectionId int
AS --Generated--
BEGIN

	DELETE FROM	[Data].[ContentInspection]
	WHERE	[ContentInspectionId] = @ContentInspectionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ContentInspection_Exists]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ContentInspection_Exists]

GO--

CREATE PROCEDURE [Data].[ContentInspection_Exists]
			@ContentInspectionId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	IF EXISTS(
		SELECT * FROM [Data].[ContentInspection]
		WHERE	[ContentInspectionId] = @ContentInspectionId
	) BEGIN
		SELECT CAST(1 as bit) as [Exists]
	END ELSE BEGIN
		SELECT CAST(0 as bit) as [Exists]
	END
END

GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ContentInspection_Insert]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ContentInspection_Insert]

GO--

CREATE PROCEDURE [Data].[ContentInspection_Insert]
			@IsArchived bit,
			@IsBeingProposed bit,
			@ProposedByUserId int,
			@ConfirmedByUserId int,
			@FalseInfoCount int,
			@TrueInfoCount int,
			@AdminInpsected bit,
			@DateLastChecked datetime,
			@SourceUrl varchar(250)
AS --Generated--
BEGIN

	INSERT INTO [Data].[ContentInspection] (

			[IsArchived],
			[IsBeingProposed],
			[ProposedByUserId],
			[ConfirmedByUserId],
			[FalseInfoCount],
			[TrueInfoCount],
			[AdminInpsected],
			[DateLastChecked],
			[SourceUrl]
	) VALUES (

			@IsArchived,
			@IsBeingProposed,
			@ProposedByUserId,
			@ConfirmedByUserId,
			@FalseInfoCount,
			@TrueInfoCount,
			@AdminInpsected,
			@DateLastChecked,
			@SourceUrl
	)

	SELECT CAST(SCOPE_IDENTITY() AS int) AS [ContentInspectionId]
END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ContentInspection_Search]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ContentInspection_Search]

GO--

CREATE PROCEDURE [Data].[ContentInspection_Search] 
			@SourceUrl varchar(250) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[ContentInspectionId],
			[IsArchived],
			[IsBeingProposed],
			[ProposedByUserId],
			[ConfirmedByUserId],
			[FalseInfoCount],
			[TrueInfoCount],
			[AdminInpsected],
			[DateLastChecked],
			[SourceUrl]
	FROM	[Data].[ContentInspection]
	WHERE	(@SourceUrl IS NULL OR [SourceUrl] LIKE '%' + @SourceUrl + '%')

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ContentInspection_SelectAll]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ContentInspection_SelectAll]

GO--

CREATE PROCEDURE [Data].[ContentInspection_SelectAll]
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[ContentInspectionId],
			[IsArchived],
			[IsBeingProposed],
			[ProposedByUserId],
			[ConfirmedByUserId],
			[FalseInfoCount],
			[TrueInfoCount],
			[AdminInpsected],
			[DateLastChecked],
			[SourceUrl]
	FROM	[Data].[ContentInspection]

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ContentInspection_SelectBy_ConfirmedByUserId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ContentInspection_SelectBy_ConfirmedByUserId]

GO--

CREATE PROCEDURE [Data].[ContentInspection_SelectBy_ConfirmedByUserId] 
			@ConfirmedByUserId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[ContentInspectionId],
			[IsArchived],
			[IsBeingProposed],
			[ProposedByUserId],
			[ConfirmedByUserId],
			[FalseInfoCount],
			[TrueInfoCount],
			[AdminInpsected],
			[DateLastChecked],
			[SourceUrl]
	FROM	[Data].[ContentInspection]
	WHERE	[ConfirmedByUserId] = @ConfirmedByUserId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ContentInspection_SelectBy_ContentInspectionId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ContentInspection_SelectBy_ContentInspectionId]

GO--

CREATE PROCEDURE [Data].[ContentInspection_SelectBy_ContentInspectionId] 
			@ContentInspectionId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[ContentInspectionId],
			[IsArchived],
			[IsBeingProposed],
			[ProposedByUserId],
			[ConfirmedByUserId],
			[FalseInfoCount],
			[TrueInfoCount],
			[AdminInpsected],
			[DateLastChecked],
			[SourceUrl]
	FROM	[Data].[ContentInspection]
	WHERE	[ContentInspectionId] = @ContentInspectionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ContentInspection_SelectBy_ProposedByUserId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ContentInspection_SelectBy_ProposedByUserId]

GO--

CREATE PROCEDURE [Data].[ContentInspection_SelectBy_ProposedByUserId] 
			@ProposedByUserId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[ContentInspectionId],
			[IsArchived],
			[IsBeingProposed],
			[ProposedByUserId],
			[ConfirmedByUserId],
			[FalseInfoCount],
			[TrueInfoCount],
			[AdminInpsected],
			[DateLastChecked],
			[SourceUrl]
	FROM	[Data].[ContentInspection]
	WHERE	[ProposedByUserId] = @ProposedByUserId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ContentInspection_Update]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ContentInspection_Update]

GO--

CREATE PROCEDURE [Data].[ContentInspection_Update] 
			@ContentInspectionId int,
			@IsArchived bit,
			@IsBeingProposed bit,
			@ProposedByUserId int,
			@ConfirmedByUserId int,
			@FalseInfoCount int,
			@TrueInfoCount int,
			@AdminInpsected bit,
			@DateLastChecked datetime,
			@SourceUrl varchar(250)
AS --Generated--
BEGIN

	UPDATE	[Data].[ContentInspection] SET 
			[IsArchived] = @IsArchived,
			[IsBeingProposed] = @IsBeingProposed,
			[ProposedByUserId] = @ProposedByUserId,
			[ConfirmedByUserId] = @ConfirmedByUserId,
			[FalseInfoCount] = @FalseInfoCount,
			[TrueInfoCount] = @TrueInfoCount,
			[AdminInpsected] = @AdminInpsected,
			[DateLastChecked] = @DateLastChecked,
			[SourceUrl] = @SourceUrl
	WHERE	[ContentInspectionId] = @ContentInspectionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Election_Delete]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Election_Delete]

GO--

CREATE PROCEDURE [Data].[Election_Delete] 
			@ElectionId int
AS --Generated--
BEGIN

	DELETE FROM	[Data].[Election]
	WHERE	[ElectionId] = @ElectionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Election_Exists]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Election_Exists]

GO--

CREATE PROCEDURE [Data].[Election_Exists]
			@ElectionId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	IF EXISTS(
		SELECT * FROM [Data].[Election]
		WHERE	[ElectionId] = @ElectionId
	) BEGIN
		SELECT CAST(1 as bit) as [Exists]
	END ELSE BEGIN
		SELECT CAST(0 as bit) as [Exists]
	END
END

GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Election_Insert]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Election_Insert]

GO--

CREATE PROCEDURE [Data].[Election_Insert]
			@ContentInspectionId int,
			@ElectionLevelId int,
			@LocationId int,
			@VotingDate datetime
AS --Generated--
BEGIN

	INSERT INTO [Data].[Election] (

			[ContentInspectionId],
			[ElectionLevelId],
			[LocationId],
			[VotingDate]
	) VALUES (

			@ContentInspectionId,
			@ElectionLevelId,
			@LocationId,
			@VotingDate
	)

	SELECT CAST(SCOPE_IDENTITY() AS int) AS [ElectionId]
END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Election_SelectAll]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Election_SelectAll]

GO--

CREATE PROCEDURE [Data].[Election_SelectAll]
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[ElectionId],
			[ContentInspectionId],
			[ElectionLevelId],
			[LocationId],
			[VotingDate]
	FROM	[Data].[Election]

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Election_SelectBy_ContentInspectionId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Election_SelectBy_ContentInspectionId]

GO--

CREATE PROCEDURE [Data].[Election_SelectBy_ContentInspectionId] 
			@ContentInspectionId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[ElectionId],
			[ContentInspectionId],
			[ElectionLevelId],
			[LocationId],
			[VotingDate]
	FROM	[Data].[Election]
	WHERE	[ContentInspectionId] = @ContentInspectionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Election_SelectBy_ElectionId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Election_SelectBy_ElectionId]

GO--

CREATE PROCEDURE [Data].[Election_SelectBy_ElectionId] 
			@ElectionId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[ElectionId],
			[ContentInspectionId],
			[ElectionLevelId],
			[LocationId],
			[VotingDate]
	FROM	[Data].[Election]
	WHERE	[ElectionId] = @ElectionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Election_SelectBy_ElectionLevelId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Election_SelectBy_ElectionLevelId]

GO--

CREATE PROCEDURE [Data].[Election_SelectBy_ElectionLevelId] 
			@ElectionLevelId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[ElectionId],
			[ContentInspectionId],
			[ElectionLevelId],
			[LocationId],
			[VotingDate]
	FROM	[Data].[Election]
	WHERE	[ElectionLevelId] = @ElectionLevelId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Election_SelectBy_LocationId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Election_SelectBy_LocationId]

GO--

CREATE PROCEDURE [Data].[Election_SelectBy_LocationId] 
			@LocationId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[ElectionId],
			[ContentInspectionId],
			[ElectionLevelId],
			[LocationId],
			[VotingDate]
	FROM	[Data].[Election]
	WHERE	[LocationId] = @LocationId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Election_Update]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Election_Update]

GO--

CREATE PROCEDURE [Data].[Election_Update] 
			@ElectionId int,
			@ContentInspectionId int,
			@ElectionLevelId int,
			@LocationId int,
			@VotingDate datetime
AS --Generated--
BEGIN

	UPDATE	[Data].[Election] SET 
			[ContentInspectionId] = @ContentInspectionId,
			[ElectionLevelId] = @ElectionLevelId,
			[LocationId] = @LocationId,
			[VotingDate] = @VotingDate
	WHERE	[ElectionId] = @ElectionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionCandidate_Delete]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionCandidate_Delete]

GO--

CREATE PROCEDURE [Data].[ElectionCandidate_Delete] 
			@ElectionCandidateId int
AS --Generated--
BEGIN

	DELETE FROM	[Data].[ElectionCandidate]
	WHERE	[ElectionCandidateId] = @ElectionCandidateId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionCandidate_Exists]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionCandidate_Exists]

GO--

CREATE PROCEDURE [Data].[ElectionCandidate_Exists]
			@ElectionCandidateId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	IF EXISTS(
		SELECT * FROM [Data].[ElectionCandidate]
		WHERE	[ElectionCandidateId] = @ElectionCandidateId
	) BEGIN
		SELECT CAST(1 as bit) as [Exists]
	END ELSE BEGIN
		SELECT CAST(0 as bit) as [Exists]
	END
END

GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionCandidate_Insert]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionCandidate_Insert]

GO--

CREATE PROCEDURE [Data].[ElectionCandidate_Insert]
			@ContentInspectionId int,
			@CandidateId int,
			@ElectionId int,
			@IsWinner bit,
			@ReportedVoteCount bit
AS --Generated--
BEGIN

	INSERT INTO [Data].[ElectionCandidate] (

			[ContentInspectionId],
			[CandidateId],
			[ElectionId],
			[IsWinner],
			[ReportedVoteCount]
	) VALUES (

			@ContentInspectionId,
			@CandidateId,
			@ElectionId,
			@IsWinner,
			@ReportedVoteCount
	)

	SELECT CAST(SCOPE_IDENTITY() AS int) AS [ElectionCandidateId]
END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionCandidate_SelectAll]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionCandidate_SelectAll]

GO--

CREATE PROCEDURE [Data].[ElectionCandidate_SelectAll]
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[ElectionCandidateId],
			[ContentInspectionId],
			[CandidateId],
			[ElectionId],
			[IsWinner],
			[ReportedVoteCount]
	FROM	[Data].[ElectionCandidate]

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionCandidate_SelectBy_CandidateId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionCandidate_SelectBy_CandidateId]

GO--

CREATE PROCEDURE [Data].[ElectionCandidate_SelectBy_CandidateId] 
			@CandidateId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[ElectionCandidateId],
			[ContentInspectionId],
			[CandidateId],
			[ElectionId],
			[IsWinner],
			[ReportedVoteCount]
	FROM	[Data].[ElectionCandidate]
	WHERE	[CandidateId] = @CandidateId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionCandidate_SelectBy_ContentInspectionId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionCandidate_SelectBy_ContentInspectionId]

GO--

CREATE PROCEDURE [Data].[ElectionCandidate_SelectBy_ContentInspectionId] 
			@ContentInspectionId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[ElectionCandidateId],
			[ContentInspectionId],
			[CandidateId],
			[ElectionId],
			[IsWinner],
			[ReportedVoteCount]
	FROM	[Data].[ElectionCandidate]
	WHERE	[ContentInspectionId] = @ContentInspectionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionCandidate_SelectBy_ElectionCandidateId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionCandidate_SelectBy_ElectionCandidateId]

GO--

CREATE PROCEDURE [Data].[ElectionCandidate_SelectBy_ElectionCandidateId] 
			@ElectionCandidateId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[ElectionCandidateId],
			[ContentInspectionId],
			[CandidateId],
			[ElectionId],
			[IsWinner],
			[ReportedVoteCount]
	FROM	[Data].[ElectionCandidate]
	WHERE	[ElectionCandidateId] = @ElectionCandidateId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionCandidate_SelectBy_ElectionId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionCandidate_SelectBy_ElectionId]

GO--

CREATE PROCEDURE [Data].[ElectionCandidate_SelectBy_ElectionId] 
			@ElectionId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[ElectionCandidateId],
			[ContentInspectionId],
			[CandidateId],
			[ElectionId],
			[IsWinner],
			[ReportedVoteCount]
	FROM	[Data].[ElectionCandidate]
	WHERE	[ElectionId] = @ElectionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionCandidate_Update]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionCandidate_Update]

GO--

CREATE PROCEDURE [Data].[ElectionCandidate_Update] 
			@ElectionCandidateId int,
			@ContentInspectionId int,
			@CandidateId int,
			@ElectionId int,
			@IsWinner bit,
			@ReportedVoteCount bit
AS --Generated--
BEGIN

	UPDATE	[Data].[ElectionCandidate] SET 
			[ContentInspectionId] = @ContentInspectionId,
			[CandidateId] = @CandidateId,
			[ElectionId] = @ElectionId,
			[IsWinner] = @IsWinner,
			[ReportedVoteCount] = @ReportedVoteCount
	WHERE	[ElectionCandidateId] = @ElectionCandidateId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionLevel_Delete]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionLevel_Delete]

GO--

CREATE PROCEDURE [Data].[ElectionLevel_Delete] 
			@ElectionLevelId int
AS --Generated--
BEGIN

	DELETE FROM	[Data].[ElectionLevel]
	WHERE	[ElectionLevelId] = @ElectionLevelId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionLevel_Exists]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionLevel_Exists]

GO--

CREATE PROCEDURE [Data].[ElectionLevel_Exists]
			@ElectionLevelId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	IF EXISTS(
		SELECT * FROM [Data].[ElectionLevel]
		WHERE	[ElectionLevelId] = @ElectionLevelId
	) BEGIN
		SELECT CAST(1 as bit) as [Exists]
	END ELSE BEGIN
		SELECT CAST(0 as bit) as [Exists]
	END
END

GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionLevel_Insert]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionLevel_Insert]

GO--

CREATE PROCEDURE [Data].[ElectionLevel_Insert]
			@ContentInspectionId int,
			@ElectionLevelTitle varchar(150)
AS --Generated--
BEGIN

	INSERT INTO [Data].[ElectionLevel] (

			[ContentInspectionId],
			[ElectionLevelTitle]
	) VALUES (

			@ContentInspectionId,
			@ElectionLevelTitle
	)

	SELECT CAST(SCOPE_IDENTITY() AS int) AS [ElectionLevelId]
END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionLevel_List]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionLevel_List]

GO--

CREATE PROCEDURE [Data].[ElectionLevel_List] 
			@ElectionLevelTitle varchar(150) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
		[ElectionLevelId] as [ListKey],
		[ElectionLevelTitle] AS [ListLabel]			
	FROM	[Data].[ElectionLevel]
	WHERE	(@ElectionLevelTitle IS NULL OR [ElectionLevelTitle] LIKE '%' + @ElectionLevelTitle + '%')
	ORDER BY [ListLabel] ASC

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionLevel_Search]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionLevel_Search]

GO--

CREATE PROCEDURE [Data].[ElectionLevel_Search] 
			@ElectionLevelTitle varchar(150) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[ElectionLevelId],
			[ContentInspectionId],
			[ElectionLevelTitle]
	FROM	[Data].[ElectionLevel]
	WHERE	(@ElectionLevelTitle IS NULL OR [ElectionLevelTitle] LIKE '%' + @ElectionLevelTitle + '%')

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionLevel_SelectAll]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionLevel_SelectAll]

GO--

CREATE PROCEDURE [Data].[ElectionLevel_SelectAll]
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[ElectionLevelId],
			[ContentInspectionId],
			[ElectionLevelTitle]
	FROM	[Data].[ElectionLevel]

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionLevel_SelectBy_ContentInspectionId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionLevel_SelectBy_ContentInspectionId]

GO--

CREATE PROCEDURE [Data].[ElectionLevel_SelectBy_ContentInspectionId] 
			@ContentInspectionId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[ElectionLevelId],
			[ContentInspectionId],
			[ElectionLevelTitle]
	FROM	[Data].[ElectionLevel]
	WHERE	[ContentInspectionId] = @ContentInspectionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionLevel_SelectBy_ElectionLevelId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionLevel_SelectBy_ElectionLevelId]

GO--

CREATE PROCEDURE [Data].[ElectionLevel_SelectBy_ElectionLevelId] 
			@ElectionLevelId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[ElectionLevelId],
			[ContentInspectionId],
			[ElectionLevelTitle]
	FROM	[Data].[ElectionLevel]
	WHERE	[ElectionLevelId] = @ElectionLevelId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionLevel_Update]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionLevel_Update]

GO--

CREATE PROCEDURE [Data].[ElectionLevel_Update] 
			@ElectionLevelId int,
			@ContentInspectionId int,
			@ElectionLevelTitle varchar(150)
AS --Generated--
BEGIN

	UPDATE	[Data].[ElectionLevel] SET 
			[ContentInspectionId] = @ContentInspectionId,
			[ElectionLevelTitle] = @ElectionLevelTitle
	WHERE	[ElectionLevelId] = @ElectionLevelId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionLevelMetaDataXref_Delete]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionLevelMetaDataXref_Delete]

GO--

CREATE PROCEDURE [Data].[ElectionLevelMetaDataXref_Delete] 
			@ElectionLevelMetaDataXrefId int
AS --Generated--
BEGIN

	DELETE FROM	[Data].[ElectionLevelMetaDataXref]
	WHERE	[ElectionLevelMetaDataXrefId] = @ElectionLevelMetaDataXrefId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionLevelMetaDataXref_Exists]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionLevelMetaDataXref_Exists]

GO--

CREATE PROCEDURE [Data].[ElectionLevelMetaDataXref_Exists]
			@ElectionLevelMetaDataXrefId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	IF EXISTS(
		SELECT * FROM [Data].[ElectionLevelMetaDataXref]
		WHERE	[ElectionLevelMetaDataXrefId] = @ElectionLevelMetaDataXrefId
	) BEGIN
		SELECT CAST(1 as bit) as [Exists]
	END ELSE BEGIN
		SELECT CAST(0 as bit) as [Exists]
	END
END

GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionLevelMetaDataXref_Insert]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionLevelMetaDataXref_Insert]

GO--

CREATE PROCEDURE [Data].[ElectionLevelMetaDataXref_Insert]
			@ContentInspectionId int,
			@ElectionLevelId int,
			@MetaDataId nchar(10) = NULL
AS --Generated--
BEGIN

	INSERT INTO [Data].[ElectionLevelMetaDataXref] (

			[ContentInspectionId],
			[ElectionLevelId],
			[MetaDataId]
	) VALUES (

			@ContentInspectionId,
			@ElectionLevelId,
			@MetaDataId
	)

	SELECT CAST(SCOPE_IDENTITY() AS int) AS [ElectionLevelMetaDataXrefId]
END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionLevelMetaDataXref_Search]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionLevelMetaDataXref_Search]

GO--

CREATE PROCEDURE [Data].[ElectionLevelMetaDataXref_Search] 
			@MetaDataId nchar(10) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[ElectionLevelMetaDataXrefId],
			[ContentInspectionId],
			[ElectionLevelId],
			[MetaDataId]
	FROM	[Data].[ElectionLevelMetaDataXref]
	WHERE	(@MetaDataId IS NULL OR [MetaDataId] LIKE '%' + @MetaDataId + '%')

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionLevelMetaDataXref_SelectAll]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionLevelMetaDataXref_SelectAll]

GO--

CREATE PROCEDURE [Data].[ElectionLevelMetaDataXref_SelectAll]
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[ElectionLevelMetaDataXrefId],
			[ContentInspectionId],
			[ElectionLevelId],
			[MetaDataId]
	FROM	[Data].[ElectionLevelMetaDataXref]

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionLevelMetaDataXref_SelectBy_ContentInspectionId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionLevelMetaDataXref_SelectBy_ContentInspectionId]

GO--

CREATE PROCEDURE [Data].[ElectionLevelMetaDataXref_SelectBy_ContentInspectionId] 
			@ContentInspectionId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[ElectionLevelMetaDataXrefId],
			[ContentInspectionId],
			[ElectionLevelId],
			[MetaDataId]
	FROM	[Data].[ElectionLevelMetaDataXref]
	WHERE	[ContentInspectionId] = @ContentInspectionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionLevelMetaDataXref_SelectBy_ElectionLevelId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionLevelMetaDataXref_SelectBy_ElectionLevelId]

GO--

CREATE PROCEDURE [Data].[ElectionLevelMetaDataXref_SelectBy_ElectionLevelId] 
			@ElectionLevelId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[ElectionLevelMetaDataXrefId],
			[ContentInspectionId],
			[ElectionLevelId],
			[MetaDataId]
	FROM	[Data].[ElectionLevelMetaDataXref]
	WHERE	[ElectionLevelId] = @ElectionLevelId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionLevelMetaDataXref_SelectBy_ElectionLevelMetaDataXrefId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionLevelMetaDataXref_SelectBy_ElectionLevelMetaDataXrefId]

GO--

CREATE PROCEDURE [Data].[ElectionLevelMetaDataXref_SelectBy_ElectionLevelMetaDataXrefId] 
			@ElectionLevelMetaDataXrefId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[ElectionLevelMetaDataXrefId],
			[ContentInspectionId],
			[ElectionLevelId],
			[MetaDataId]
	FROM	[Data].[ElectionLevelMetaDataXref]
	WHERE	[ElectionLevelMetaDataXrefId] = @ElectionLevelMetaDataXrefId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionLevelMetaDataXref_SelectBy_MetaDataId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionLevelMetaDataXref_SelectBy_MetaDataId]

GO--

CREATE PROCEDURE [Data].[ElectionLevelMetaDataXref_SelectBy_MetaDataId] 
			@MetaDataId nchar(10)
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[ElectionLevelMetaDataXrefId],
			[ContentInspectionId],
			[ElectionLevelId],
			[MetaDataId]
	FROM	[Data].[ElectionLevelMetaDataXref]
	WHERE	[MetaDataId] = @MetaDataId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[ElectionLevelMetaDataXref_Update]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[ElectionLevelMetaDataXref_Update]

GO--

CREATE PROCEDURE [Data].[ElectionLevelMetaDataXref_Update] 
			@ElectionLevelMetaDataXrefId int,
			@ContentInspectionId int,
			@ElectionLevelId int,
			@MetaDataId nchar(10) = NULL
AS --Generated--
BEGIN

	UPDATE	[Data].[ElectionLevelMetaDataXref] SET 
			[ContentInspectionId] = @ContentInspectionId,
			[ElectionLevelId] = @ElectionLevelId,
			[MetaDataId] = @MetaDataId
	WHERE	[ElectionLevelMetaDataXrefId] = @ElectionLevelMetaDataXrefId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Location_Delete]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Location_Delete]

GO--

CREATE PROCEDURE [Data].[Location_Delete] 
			@LocationId int
AS --Generated--
BEGIN

	DELETE FROM	[Data].[Location]
	WHERE	[LocationId] = @LocationId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Location_Exists]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Location_Exists]

GO--

CREATE PROCEDURE [Data].[Location_Exists]
			@LocationId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	IF EXISTS(
		SELECT * FROM [Data].[Location]
		WHERE	[LocationId] = @LocationId
	) BEGIN
		SELECT CAST(1 as bit) as [Exists]
	END ELSE BEGIN
		SELECT CAST(0 as bit) as [Exists]
	END
END

GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Location_Insert]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Location_Insert]

GO--

CREATE PROCEDURE [Data].[Location_Insert]
			@ContentInspectionId int,
			@LocationName varchar(150)
AS --Generated--
BEGIN

	INSERT INTO [Data].[Location] (

			[ContentInspectionId],
			[LocationName]
	) VALUES (

			@ContentInspectionId,
			@LocationName
	)

	SELECT CAST(SCOPE_IDENTITY() AS int) AS [LocationId]
END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Location_List]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Location_List]

GO--

CREATE PROCEDURE [Data].[Location_List] 
			@LocationName varchar(150) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
		[LocationId] as [ListKey],
		[LocationName] AS [ListLabel]			
	FROM	[Data].[Location]
	WHERE	(@LocationName IS NULL OR [LocationName] LIKE '%' + @LocationName + '%')
	ORDER BY [ListLabel] ASC

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Location_Search]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Location_Search]

GO--

CREATE PROCEDURE [Data].[Location_Search] 
			@LocationName varchar(150) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[LocationId],
			[ContentInspectionId],
			[LocationName]
	FROM	[Data].[Location]
	WHERE	(@LocationName IS NULL OR [LocationName] LIKE '%' + @LocationName + '%')

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Location_SelectAll]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Location_SelectAll]

GO--

CREATE PROCEDURE [Data].[Location_SelectAll]
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[LocationId],
			[ContentInspectionId],
			[LocationName]
	FROM	[Data].[Location]

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Location_SelectBy_ContentInspectionId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Location_SelectBy_ContentInspectionId]

GO--

CREATE PROCEDURE [Data].[Location_SelectBy_ContentInspectionId] 
			@ContentInspectionId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[LocationId],
			[ContentInspectionId],
			[LocationName]
	FROM	[Data].[Location]
	WHERE	[ContentInspectionId] = @ContentInspectionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Location_SelectBy_LocationId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Location_SelectBy_LocationId]

GO--

CREATE PROCEDURE [Data].[Location_SelectBy_LocationId] 
			@LocationId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[LocationId],
			[ContentInspectionId],
			[LocationName]
	FROM	[Data].[Location]
	WHERE	[LocationId] = @LocationId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Location_Update]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Location_Update]

GO--

CREATE PROCEDURE [Data].[Location_Update] 
			@LocationId int,
			@ContentInspectionId int,
			@LocationName varchar(150)
AS --Generated--
BEGIN

	UPDATE	[Data].[Location] SET 
			[ContentInspectionId] = @ContentInspectionId,
			[LocationName] = @LocationName
	WHERE	[LocationId] = @LocationId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[MetaData_Delete]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[MetaData_Delete]

GO--

CREATE PROCEDURE [Data].[MetaData_Delete] 
			@MetaDataId int
AS --Generated--
BEGIN

	DELETE FROM	[Data].[MetaData]
	WHERE	[MetaDataId] = @MetaDataId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[MetaData_Exists]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[MetaData_Exists]

GO--

CREATE PROCEDURE [Data].[MetaData_Exists]
			@MetaDataId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	IF EXISTS(
		SELECT * FROM [Data].[MetaData]
		WHERE	[MetaDataId] = @MetaDataId
	) BEGIN
		SELECT CAST(1 as bit) as [Exists]
	END ELSE BEGIN
		SELECT CAST(0 as bit) as [Exists]
	END
END

GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[MetaData_Insert]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[MetaData_Insert]

GO--

CREATE PROCEDURE [Data].[MetaData_Insert]
			@ContentInspectionId int,
			@MetaDataName varchar(150),
			@IsRequired bit,
			@AppliesAtAllLevels bit,
			@AppliesToCandidates bit,
			@AppliesToOrganizations bit
AS --Generated--
BEGIN

	INSERT INTO [Data].[MetaData] (

			[ContentInspectionId],
			[MetaDataName],
			[IsRequired],
			[AppliesAtAllLevels],
			[AppliesToCandidates],
			[AppliesToOrganizations]
	) VALUES (

			@ContentInspectionId,
			@MetaDataName,
			@IsRequired,
			@AppliesAtAllLevels,
			@AppliesToCandidates,
			@AppliesToOrganizations
	)

	SELECT CAST(SCOPE_IDENTITY() AS int) AS [MetaDataId]
END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[MetaData_List]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[MetaData_List]

GO--

CREATE PROCEDURE [Data].[MetaData_List] 
			@MetaDataName varchar(150) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
		[MetaDataId] as [ListKey],
		[MetaDataName] AS [ListLabel]			
	FROM	[Data].[MetaData]
	WHERE	(@MetaDataName IS NULL OR [MetaDataName] LIKE '%' + @MetaDataName + '%')
	ORDER BY [ListLabel] ASC

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[MetaData_Search]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[MetaData_Search]

GO--

CREATE PROCEDURE [Data].[MetaData_Search] 
			@MetaDataName varchar(150) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[MetaDataId],
			[ContentInspectionId],
			[MetaDataName],
			[IsRequired],
			[AppliesAtAllLevels],
			[AppliesToCandidates],
			[AppliesToOrganizations]
	FROM	[Data].[MetaData]
	WHERE	(@MetaDataName IS NULL OR [MetaDataName] LIKE '%' + @MetaDataName + '%')

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[MetaData_SelectAll]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[MetaData_SelectAll]

GO--

CREATE PROCEDURE [Data].[MetaData_SelectAll]
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[MetaDataId],
			[ContentInspectionId],
			[MetaDataName],
			[IsRequired],
			[AppliesAtAllLevels],
			[AppliesToCandidates],
			[AppliesToOrganizations]
	FROM	[Data].[MetaData]

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[MetaData_SelectBy_ContentInspectionId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[MetaData_SelectBy_ContentInspectionId]

GO--

CREATE PROCEDURE [Data].[MetaData_SelectBy_ContentInspectionId] 
			@ContentInspectionId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[MetaDataId],
			[ContentInspectionId],
			[MetaDataName],
			[IsRequired],
			[AppliesAtAllLevels],
			[AppliesToCandidates],
			[AppliesToOrganizations]
	FROM	[Data].[MetaData]
	WHERE	[ContentInspectionId] = @ContentInspectionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[MetaData_SelectBy_MetaDataId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[MetaData_SelectBy_MetaDataId]

GO--

CREATE PROCEDURE [Data].[MetaData_SelectBy_MetaDataId] 
			@MetaDataId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[MetaDataId],
			[ContentInspectionId],
			[MetaDataName],
			[IsRequired],
			[AppliesAtAllLevels],
			[AppliesToCandidates],
			[AppliesToOrganizations]
	FROM	[Data].[MetaData]
	WHERE	[MetaDataId] = @MetaDataId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[MetaData_Update]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[MetaData_Update]

GO--

CREATE PROCEDURE [Data].[MetaData_Update] 
			@MetaDataId int,
			@ContentInspectionId int,
			@MetaDataName varchar(150),
			@IsRequired bit,
			@AppliesAtAllLevels bit,
			@AppliesToCandidates bit,
			@AppliesToOrganizations bit
AS --Generated--
BEGIN

	UPDATE	[Data].[MetaData] SET 
			[ContentInspectionId] = @ContentInspectionId,
			[MetaDataName] = @MetaDataName,
			[IsRequired] = @IsRequired,
			[AppliesAtAllLevels] = @AppliesAtAllLevels,
			[AppliesToCandidates] = @AppliesToCandidates,
			[AppliesToOrganizations] = @AppliesToOrganizations
	WHERE	[MetaDataId] = @MetaDataId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Organization_Delete]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Organization_Delete]

GO--

CREATE PROCEDURE [Data].[Organization_Delete] 
			@OrganizationId int
AS --Generated--
BEGIN

	DELETE FROM	[Data].[Organization]
	WHERE	[OrganizationId] = @OrganizationId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Organization_Exists]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Organization_Exists]

GO--

CREATE PROCEDURE [Data].[Organization_Exists]
			@OrganizationId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	IF EXISTS(
		SELECT * FROM [Data].[Organization]
		WHERE	[OrganizationId] = @OrganizationId
	) BEGIN
		SELECT CAST(1 as bit) as [Exists]
	END ELSE BEGIN
		SELECT CAST(0 as bit) as [Exists]
	END
END

GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Organization_Insert]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Organization_Insert]

GO--

CREATE PROCEDURE [Data].[Organization_Insert]
			@ContentInspectionId int,
			@OrganizationName varchar(250)
AS --Generated--
BEGIN

	INSERT INTO [Data].[Organization] (

			[ContentInspectionId],
			[OrganizationName]
	) VALUES (

			@ContentInspectionId,
			@OrganizationName
	)

	SELECT CAST(SCOPE_IDENTITY() AS int) AS [OrganizationId]
END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Organization_List]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Organization_List]

GO--

CREATE PROCEDURE [Data].[Organization_List] 
			@OrganizationName varchar(250) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
		[OrganizationId] as [ListKey],
		[OrganizationName] AS [ListLabel]			
	FROM	[Data].[Organization]
	WHERE	(@OrganizationName IS NULL OR [OrganizationName] LIKE '%' + @OrganizationName + '%')
	ORDER BY [ListLabel] ASC

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Organization_Search]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Organization_Search]

GO--

CREATE PROCEDURE [Data].[Organization_Search] 
			@OrganizationName varchar(250) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[OrganizationId],
			[ContentInspectionId],
			[OrganizationName]
	FROM	[Data].[Organization]
	WHERE	(@OrganizationName IS NULL OR [OrganizationName] LIKE '%' + @OrganizationName + '%')

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Organization_SelectAll]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Organization_SelectAll]

GO--

CREATE PROCEDURE [Data].[Organization_SelectAll]
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[OrganizationId],
			[ContentInspectionId],
			[OrganizationName]
	FROM	[Data].[Organization]

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Organization_SelectBy_ContentInspectionId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Organization_SelectBy_ContentInspectionId]

GO--

CREATE PROCEDURE [Data].[Organization_SelectBy_ContentInspectionId] 
			@ContentInspectionId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[OrganizationId],
			[ContentInspectionId],
			[OrganizationName]
	FROM	[Data].[Organization]
	WHERE	[ContentInspectionId] = @ContentInspectionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Organization_SelectBy_OrganizationId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Organization_SelectBy_OrganizationId]

GO--

CREATE PROCEDURE [Data].[Organization_SelectBy_OrganizationId] 
			@OrganizationId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[OrganizationId],
			[ContentInspectionId],
			[OrganizationName]
	FROM	[Data].[Organization]
	WHERE	[OrganizationId] = @OrganizationId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Organization_Update]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Organization_Update]

GO--

CREATE PROCEDURE [Data].[Organization_Update] 
			@OrganizationId int,
			@ContentInspectionId int,
			@OrganizationName varchar(250)
AS --Generated--
BEGIN

	UPDATE	[Data].[Organization] SET 
			[ContentInspectionId] = @ContentInspectionId,
			[OrganizationName] = @OrganizationName
	WHERE	[OrganizationId] = @OrganizationId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[OrganizationMetaData_Delete]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[OrganizationMetaData_Delete]

GO--

CREATE PROCEDURE [Data].[OrganizationMetaData_Delete] 
			@OrganizationMetaDataId int
AS --Generated--
BEGIN

	DELETE FROM	[Data].[OrganizationMetaData]
	WHERE	[OrganizationMetaDataId] = @OrganizationMetaDataId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[OrganizationMetaData_Exists]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[OrganizationMetaData_Exists]

GO--

CREATE PROCEDURE [Data].[OrganizationMetaData_Exists]
			@OrganizationMetaDataId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	IF EXISTS(
		SELECT * FROM [Data].[OrganizationMetaData]
		WHERE	[OrganizationMetaDataId] = @OrganizationMetaDataId
	) BEGIN
		SELECT CAST(1 as bit) as [Exists]
	END ELSE BEGIN
		SELECT CAST(0 as bit) as [Exists]
	END
END

GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[OrganizationMetaData_Insert]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[OrganizationMetaData_Insert]

GO--

CREATE PROCEDURE [Data].[OrganizationMetaData_Insert]
			@ContentInspectionId int,
			@OrganizationId int,
			@MetaDataId int,
			@MetaDataValue varchar(max)
AS --Generated--
BEGIN

	INSERT INTO [Data].[OrganizationMetaData] (

			[ContentInspectionId],
			[OrganizationId],
			[MetaDataId],
			[MetaDataValue]
	) VALUES (

			@ContentInspectionId,
			@OrganizationId,
			@MetaDataId,
			@MetaDataValue
	)

	SELECT CAST(SCOPE_IDENTITY() AS int) AS [OrganizationMetaDataId]
END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[OrganizationMetaData_Search]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[OrganizationMetaData_Search]

GO--

CREATE PROCEDURE [Data].[OrganizationMetaData_Search] 
			@MetaDataValue varchar(max) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[OrganizationMetaDataId],
			[ContentInspectionId],
			[OrganizationId],
			[MetaDataId],
			[MetaDataValue]
	FROM	[Data].[OrganizationMetaData]
	WHERE	(@MetaDataValue IS NULL OR [MetaDataValue] LIKE '%' + @MetaDataValue + '%')

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[OrganizationMetaData_SelectAll]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[OrganizationMetaData_SelectAll]

GO--

CREATE PROCEDURE [Data].[OrganizationMetaData_SelectAll]
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[OrganizationMetaDataId],
			[ContentInspectionId],
			[OrganizationId],
			[MetaDataId],
			[MetaDataValue]
	FROM	[Data].[OrganizationMetaData]

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[OrganizationMetaData_SelectBy_ContentInspectionId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[OrganizationMetaData_SelectBy_ContentInspectionId]

GO--

CREATE PROCEDURE [Data].[OrganizationMetaData_SelectBy_ContentInspectionId] 
			@ContentInspectionId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[OrganizationMetaDataId],
			[ContentInspectionId],
			[OrganizationId],
			[MetaDataId],
			[MetaDataValue]
	FROM	[Data].[OrganizationMetaData]
	WHERE	[ContentInspectionId] = @ContentInspectionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[OrganizationMetaData_SelectBy_MetaDataId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[OrganizationMetaData_SelectBy_MetaDataId]

GO--

CREATE PROCEDURE [Data].[OrganizationMetaData_SelectBy_MetaDataId] 
			@MetaDataId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[OrganizationMetaDataId],
			[ContentInspectionId],
			[OrganizationId],
			[MetaDataId],
			[MetaDataValue]
	FROM	[Data].[OrganizationMetaData]
	WHERE	[MetaDataId] = @MetaDataId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[OrganizationMetaData_SelectBy_OrganizationId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[OrganizationMetaData_SelectBy_OrganizationId]

GO--

CREATE PROCEDURE [Data].[OrganizationMetaData_SelectBy_OrganizationId] 
			@OrganizationId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[OrganizationMetaDataId],
			[ContentInspectionId],
			[OrganizationId],
			[MetaDataId],
			[MetaDataValue]
	FROM	[Data].[OrganizationMetaData]
	WHERE	[OrganizationId] = @OrganizationId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[OrganizationMetaData_SelectBy_OrganizationMetaDataId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[OrganizationMetaData_SelectBy_OrganizationMetaDataId]

GO--

CREATE PROCEDURE [Data].[OrganizationMetaData_SelectBy_OrganizationMetaDataId] 
			@OrganizationMetaDataId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[OrganizationMetaDataId],
			[ContentInspectionId],
			[OrganizationId],
			[MetaDataId],
			[MetaDataValue]
	FROM	[Data].[OrganizationMetaData]
	WHERE	[OrganizationMetaDataId] = @OrganizationMetaDataId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[OrganizationMetaData_Update]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[OrganizationMetaData_Update]

GO--

CREATE PROCEDURE [Data].[OrganizationMetaData_Update] 
			@OrganizationMetaDataId int,
			@ContentInspectionId int,
			@OrganizationId int,
			@MetaDataId int,
			@MetaDataValue varchar(max)
AS --Generated--
BEGIN

	UPDATE	[Data].[OrganizationMetaData] SET 
			[ContentInspectionId] = @ContentInspectionId,
			[OrganizationId] = @OrganizationId,
			[MetaDataId] = @MetaDataId,
			[MetaDataValue] = @MetaDataValue
	WHERE	[OrganizationMetaDataId] = @OrganizationMetaDataId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[TableVersion_Delete]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[TableVersion_Delete]

GO--

CREATE PROCEDURE [Data].[TableVersion_Delete] 
			@TableId int
AS --Generated--
BEGIN

	DELETE FROM	[Data].[TableVersion]
	WHERE	[TableId] = @TableId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[TableVersion_Exists]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[TableVersion_Exists]

GO--

CREATE PROCEDURE [Data].[TableVersion_Exists]
			@TableId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	IF EXISTS(
		SELECT * FROM [Data].[TableVersion]
		WHERE	[TableId] = @TableId
	) BEGIN
		SELECT CAST(1 as bit) as [Exists]
	END ELSE BEGIN
		SELECT CAST(0 as bit) as [Exists]
	END
END

GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[TableVersion_Insert]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[TableVersion_Insert]

GO--

CREATE PROCEDURE [Data].[TableVersion_Insert]
			@TableName varchar(150),
			@Version int
AS --Generated--
BEGIN

	INSERT INTO [Data].[TableVersion] (

			[TableName],
			[Version]
	) VALUES (

			@TableName,
			@Version
	)

	SELECT CAST(SCOPE_IDENTITY() AS int) AS [TableId]
END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[TableVersion_Search]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[TableVersion_Search]

GO--

CREATE PROCEDURE [Data].[TableVersion_Search] 
			@TableName varchar(150) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[TableId],
			[TableName],
			[Version]
	FROM	[Data].[TableVersion]
	WHERE	(@TableName IS NULL OR [TableName] LIKE '%' + @TableName + '%')

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[TableVersion_SelectAll]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[TableVersion_SelectAll]

GO--

CREATE PROCEDURE [Data].[TableVersion_SelectAll]
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[TableId],
			[TableName],
			[Version]
	FROM	[Data].[TableVersion]

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[TableVersion_SelectBy_TableId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[TableVersion_SelectBy_TableId]

GO--

CREATE PROCEDURE [Data].[TableVersion_SelectBy_TableId] 
			@TableId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[TableId],
			[TableName],
			[Version]
	FROM	[Data].[TableVersion]
	WHERE	[TableId] = @TableId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[TableVersion_Update]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[TableVersion_Update]

GO--

CREATE PROCEDURE [Data].[TableVersion_Update] 
			@TableId int,
			@TableName varchar(150),
			@Version int
AS --Generated--
BEGIN

	UPDATE	[Data].[TableVersion] SET 
			[TableName] = @TableName,
			[Version] = @Version
	WHERE	[TableId] = @TableId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Voter_Delete]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Voter_Delete]

GO--

CREATE PROCEDURE [Data].[Voter_Delete] 
			@VoterId int
AS --Generated--
BEGIN

	DELETE FROM	[Data].[Voter]
	WHERE	[VoterId] = @VoterId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Voter_Exists]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Voter_Exists]

GO--

CREATE PROCEDURE [Data].[Voter_Exists]
			@VoterId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	IF EXISTS(
		SELECT * FROM [Data].[Voter]
		WHERE	[VoterId] = @VoterId
	) BEGIN
		SELECT CAST(1 as bit) as [Exists]
	END ELSE BEGIN
		SELECT CAST(0 as bit) as [Exists]
	END
END

GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Voter_Insert]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Voter_Insert]

GO--

CREATE PROCEDURE [Data].[Voter_Insert]
			@UserId int,
			@ContentInspectionId int,
			@LocationId int,
			@VoterName varchar(150),
			@PostalCode char(6),
			@FavoriteOrganizationId int
AS --Generated--
BEGIN

	INSERT INTO [Data].[Voter] (

			[UserId],
			[ContentInspectionId],
			[LocationId],
			[VoterName],
			[PostalCode],
			[FavoriteOrganizationId]
	) VALUES (

			@UserId,
			@ContentInspectionId,
			@LocationId,
			@VoterName,
			@PostalCode,
			@FavoriteOrganizationId
	)

	SELECT CAST(SCOPE_IDENTITY() AS int) AS [VoterId]
END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Voter_List]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Voter_List]

GO--

CREATE PROCEDURE [Data].[Voter_List] 
			@VoterName varchar(150) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
		[VoterId] as [ListKey],
		[VoterName] AS [ListLabel]			
	FROM	[Data].[Voter]
	WHERE	(@VoterName IS NULL OR [VoterName] LIKE '%' + @VoterName + '%')
	ORDER BY [ListLabel] ASC

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Voter_Search]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Voter_Search]

GO--

CREATE PROCEDURE [Data].[Voter_Search] 
			@VoterName varchar(150) = NULL,
			@PostalCode char(6) = NULL
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[VoterId],
			[UserId],
			[ContentInspectionId],
			[LocationId],
			[VoterName],
			[PostalCode],
			[FavoriteOrganizationId]
	FROM	[Data].[Voter]
	WHERE	(@VoterName IS NULL OR [VoterName] LIKE '%' + @VoterName + '%')
			AND (@PostalCode IS NULL OR [PostalCode] LIKE '%' + @PostalCode + '%')

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Voter_SelectAll]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Voter_SelectAll]

GO--

CREATE PROCEDURE [Data].[Voter_SelectAll]
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[VoterId],
			[UserId],
			[ContentInspectionId],
			[LocationId],
			[VoterName],
			[PostalCode],
			[FavoriteOrganizationId]
	FROM	[Data].[Voter]

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Voter_SelectBy_ContentInspectionId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Voter_SelectBy_ContentInspectionId]

GO--

CREATE PROCEDURE [Data].[Voter_SelectBy_ContentInspectionId] 
			@ContentInspectionId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[VoterId],
			[UserId],
			[ContentInspectionId],
			[LocationId],
			[VoterName],
			[PostalCode],
			[FavoriteOrganizationId]
	FROM	[Data].[Voter]
	WHERE	[ContentInspectionId] = @ContentInspectionId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Voter_SelectBy_FavoriteOrganizationId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Voter_SelectBy_FavoriteOrganizationId]

GO--

CREATE PROCEDURE [Data].[Voter_SelectBy_FavoriteOrganizationId] 
			@FavoriteOrganizationId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[VoterId],
			[UserId],
			[ContentInspectionId],
			[LocationId],
			[VoterName],
			[PostalCode],
			[FavoriteOrganizationId]
	FROM	[Data].[Voter]
	WHERE	[FavoriteOrganizationId] = @FavoriteOrganizationId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Voter_SelectBy_LocationId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Voter_SelectBy_LocationId]

GO--

CREATE PROCEDURE [Data].[Voter_SelectBy_LocationId] 
			@LocationId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[VoterId],
			[UserId],
			[ContentInspectionId],
			[LocationId],
			[VoterName],
			[PostalCode],
			[FavoriteOrganizationId]
	FROM	[Data].[Voter]
	WHERE	[LocationId] = @LocationId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Voter_SelectBy_UserId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Voter_SelectBy_UserId]

GO--

CREATE PROCEDURE [Data].[Voter_SelectBy_UserId] 
			@UserId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[VoterId],
			[UserId],
			[ContentInspectionId],
			[LocationId],
			[VoterName],
			[PostalCode],
			[FavoriteOrganizationId]
	FROM	[Data].[Voter]
	WHERE	[UserId] = @UserId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Voter_SelectBy_VoterId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Voter_SelectBy_VoterId]

GO--

CREATE PROCEDURE [Data].[Voter_SelectBy_VoterId] 
			@VoterId int
AS --Generated--
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[VoterId],
			[UserId],
			[ContentInspectionId],
			[LocationId],
			[VoterName],
			[PostalCode],
			[FavoriteOrganizationId]
	FROM	[Data].[Voter]
	WHERE	[VoterId] = @VoterId

END
GO--
-----------------------------------------------------------------------------------
--Do not modify this file, instead use an alter proc to over-write the procedure.--
--Make sure you follow the same expected interface of parameters, and resultsets.--
-----------------------------------------------------------------------------------

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Data].[Voter_Update]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Data].[Voter_Update]

GO--

CREATE PROCEDURE [Data].[Voter_Update] 
			@VoterId int,
			@UserId int,
			@ContentInspectionId int,
			@LocationId int,
			@VoterName varchar(150),
			@PostalCode char(6),
			@FavoriteOrganizationId int
AS --Generated--
BEGIN

	UPDATE	[Data].[Voter] SET 
			[UserId] = @UserId,
			[ContentInspectionId] = @ContentInspectionId,
			[LocationId] = @LocationId,
			[VoterName] = @VoterName,
			[PostalCode] = @PostalCode,
			[FavoriteOrganizationId] = @FavoriteOrganizationId
	WHERE	[VoterId] = @VoterId

END
GO--

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Auth].[Permission_SelectBy_UserId]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Auth].[Permission_SelectBy_UserId]

GO--

CREATE PROCEDURE [Auth].[Permission_SelectBy_UserId] 
			@UserId int
AS 
BEGIN
	SET NOCOUNT ON;

	SELECT	
			[PermissionId],
			[PermissionName],
			[Title],
			[IsRead]
	FROM	[Auth].[Permission]
	WHERE	
	[PermissionId] IN -- Anonymous / All users permissions
	(	SELECT	[PermissionId]
		FROM	[Auth].[RolePermission] RP
		INNER JOIN
				[Auth].[Role] R
				ON RP.[RoleId] = R.[RoleId]
		WHERE
			(R.[ApplyToAnon] = 1 OR (R.[ApplyToAllUsers] = 1 AND @UserId > 0))
			AND R.[IsActive] = 1
	) OR [PermissionId] IN -- Specifically assigned permissions
	(	SELECT	[PermissionId]
		FROM	[Auth].[RolePermission] RP
		INNER JOIN
				[Auth].[Role] R
				ON RP.[RoleId] = R.[RoleId]
		INNER JOIN 
				[Auth].[UserRole] UR
				ON R.[RoleId] = UR.[RoleId]
		WHERE
			UR.[UserId] = @UserId
			AND R.[IsActive] = 1
	)
		

END
GO--

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Organizations_SelectAll]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Organizations_SelectAll]

GO--

CREATE PROCEDURE [Client].[Organizations_SelectAll]
AS 
BEGIN
	SET NOCOUNT ON;

	SELECT	*
	FROM	[Client].[Organizations]
	ORDER BY [OrganizationName] ASC

END
GO--

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Users_SelectByUser_Current]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Users_SelectByUser_Current]

GO--

CREATE PROCEDURE [Client].[Users_SelectByUser_Current]
	@AuthUserId int
AS
BEGIN
	SET NOCOUNT ON;

	SELECT	*
	FROM	[Client].[Users]
	WHERE	[UserId] = @AuthUserId

END
GO--

IF EXISTS(SELECT * FROM [dbo].[sysobjects] WHERE ID=object_id(N'[Client].[Users_SelectByUser_UpdateProfile]') AND OBJECTPROPERTY(id, N'IsProcedure')=1) DROP PROC [Client].[Users_SelectByUser_UpdateProfile]

GO--

CREATE PROCEDURE [Client].[Users_SelectByUser_UpdateProfile]
	@AuthUserId int,
	@DisplayName varchar(50),
	@Email varchar(100)
AS
BEGIN
	SET NOCOUNT ON;

	if( LEN(@DisplayName) <= 1 ) SET @DisplayName = 'My Name Is Invaid';


	UPDATE [Auth].[User] SET
	[DisplayName] = @DisplayName,
	[Email] = @Email
	WHERE	[UserId] = @AuthUserId


	SELECT	*
	FROM	[Client].[Users]
	WHERE	[UserId] = @AuthUserId

END
GO--
CREATE NONCLUSTERED INDEX [IX_User_UserName] 
	ON [Auth].[User] ([UserName])
CREATE NONCLUSTERED INDEX [IX_User_Email] 
	ON [Auth].[User] ([Email])
CREATE NONCLUSTERED INDEX [IX_User_UserToken] 
	ON [Auth].[User] ([UserToken])
CREATE NONCLUSTERED INDEX [IX_User_WINSID] 
	ON [Auth].[User] ([WINSID])

GO--
CREATE UNIQUE CLUSTERED INDEX [IX_Client_Candidates_CandidateId] 
	ON [Client].[Candidates] ([CandidateId])

CREATE  NONCLUSTERED INDEX [IX_Client_Candidates_ContentInspectionId] 
	ON [Client].[Candidates] ([ContentInspectionId])

CREATE  NONCLUSTERED INDEX [IX_Client_Candidates_OrganizationId] 
	ON [Client].[Candidates] ([OrganizationId])

CREATE  NONCLUSTERED INDEX [IX_Client_Candidates_ProposedByUserId] 
	ON [Client].[Candidates] ([ProposedByUserId])

CREATE  NONCLUSTERED INDEX [IX_Client_Candidates_ConfirmedByUserId] 
	ON [Client].[Candidates] ([ConfirmedByUserId])

GO--
CREATE UNIQUE CLUSTERED INDEX [IX_Client_Locations_LocationId] 
	ON [Client].[Locations] ([LocationId])

CREATE  NONCLUSTERED INDEX [IX_Client_Locations_ContentInspectionId] 
	ON [Client].[Locations] ([ContentInspectionId])

CREATE  NONCLUSTERED INDEX [IX_Client_Locations_ProposedByUserId] 
	ON [Client].[Locations] ([ProposedByUserId])

CREATE  NONCLUSTERED INDEX [IX_Client_Locations_ConfirmedByUserId] 
	ON [Client].[Locations] ([ConfirmedByUserId])

GO--
CREATE UNIQUE CLUSTERED INDEX [IX_Client_Organizations_OrganizationId] 
	ON [Client].[Organizations] ([OrganizationId])

CREATE  NONCLUSTERED INDEX [IX_Client_Organizations_ContentInspectionId] 
	ON [Client].[Organizations] ([ContentInspectionId])

CREATE  NONCLUSTERED INDEX [IX_Client_Organizations_ProposedByUserId] 
	ON [Client].[Organizations] ([ProposedByUserId])

CREATE  NONCLUSTERED INDEX [IX_Client_Organizations_ConfirmedByUserId] 
	ON [Client].[Organizations] ([ConfirmedByUserId])

GO--
CREATE  NONCLUSTERED INDEX [IX_Data_ElectionLevelMetaDataXref_MetaDataId] 
	ON [Data].[ElectionLevelMetaDataXref] ([MetaDataId])

GO--
ALTER TABLE [Auth].[RolePermission]
ADD	CONSTRAINT [FK_RolePermission_RoleId] FOREIGN KEY ([RoleId]) REFERENCES [Auth].[Role] ([RoleId])
,	CONSTRAINT [FK_RolePermission_PermissionId] FOREIGN KEY ([PermissionId]) REFERENCES [Auth].[Permission] ([PermissionId])

GO--
ALTER TABLE [Auth].[UserRole]
ADD	CONSTRAINT [FK_UserRole_RoleId] FOREIGN KEY ([RoleId]) REFERENCES [Auth].[Role] ([RoleId])
,	CONSTRAINT [FK_UserRole_UserId] FOREIGN KEY ([UserId]) REFERENCES [Auth].[User] ([UserId])

GO--
ALTER TABLE [Data].[Candidate]
ADD	CONSTRAINT [FK_Data_Candidate_UserId] FOREIGN KEY ([UserId]) REFERENCES [Auth].[User] ([UserId])

,	CONSTRAINT [FK_Data_Candidate_ContentInspectionId] FOREIGN KEY ([ContentInspectionId]) REFERENCES [Data].[ContentInspection] ([ContentInspectionId])

,	CONSTRAINT [FK_Data_Candidate_LocationId] FOREIGN KEY ([LocationId]) REFERENCES [Data].[Location] ([LocationId])

,	CONSTRAINT [FK_Data_Candidate_OrganizationId] FOREIGN KEY ([OrganizationId]) REFERENCES [Data].[Organization] ([OrganizationId])


GO--
ALTER TABLE [Data].[CandidateMetaData]
ADD	CONSTRAINT [FK_Data_CandidateMetaData_ContentInspectionId] FOREIGN KEY ([ContentInspectionId]) REFERENCES [Data].[ContentInspection] ([ContentInspectionId])

,	CONSTRAINT [FK_Data_CandidateMetaData_CandidateId] FOREIGN KEY ([CandidateId]) REFERENCES [Data].[Candidate] ([CandidateId])

,	CONSTRAINT [FK_Data_CandidateMetaData_MetaDataId] FOREIGN KEY ([MetaDataId]) REFERENCES [Data].[MetaData] ([MetaDataId])


GO--
ALTER TABLE [Data].[ContentInspection]
ADD	CONSTRAINT [FK_Data_ContentInspection_ProposedByUserId] FOREIGN KEY ([ProposedByUserId]) REFERENCES [Auth].[User] ([UserId])

,	CONSTRAINT [FK_Data_ContentInspection_ConfirmedByUserId] FOREIGN KEY ([ConfirmedByUserId]) REFERENCES [Auth].[User] ([UserId])


GO--
ALTER TABLE [Data].[Election]
ADD	CONSTRAINT [FK_Data_Election_ContentInspectionId] FOREIGN KEY ([ContentInspectionId]) REFERENCES [Data].[ContentInspection] ([ContentInspectionId])

,	CONSTRAINT [FK_Data_Election_ElectionLevelId] FOREIGN KEY ([ElectionLevelId]) REFERENCES [Data].[ElectionLevel] ([ElectionLevelId])

,	CONSTRAINT [FK_Data_Election_LocationId] FOREIGN KEY ([LocationId]) REFERENCES [Data].[Location] ([LocationId])


GO--
ALTER TABLE [Data].[ElectionCandidate]
ADD	CONSTRAINT [FK_Data_ElectionCandidate_ContentInspectionId] FOREIGN KEY ([ContentInspectionId]) REFERENCES [Data].[ContentInspection] ([ContentInspectionId])

,	CONSTRAINT [FK_Data_ElectionCandidate_CandidateId] FOREIGN KEY ([CandidateId]) REFERENCES [Data].[Candidate] ([CandidateId])

,	CONSTRAINT [FK_Data_ElectionCandidate_ElectionId] FOREIGN KEY ([ElectionId]) REFERENCES [Data].[Election] ([ElectionId])


GO--
ALTER TABLE [Data].[ElectionLevel]
ADD	CONSTRAINT [FK_Data_ElectionLevel_ContentInspectionId] FOREIGN KEY ([ContentInspectionId]) REFERENCES [Data].[ContentInspection] ([ContentInspectionId])


GO--
ALTER TABLE [Data].[ElectionLevelMetaDataXref]
ADD	CONSTRAINT [FK_Data_ElectionLevelMetaDataXref_ContentInspectionId] FOREIGN KEY ([ContentInspectionId]) REFERENCES [Data].[ContentInspection] ([ContentInspectionId])

,	CONSTRAINT [FK_Data_ElectionLevelMetaDataXref_ElectionLevelId] FOREIGN KEY ([ElectionLevelId]) REFERENCES [Data].[ElectionLevel] ([ElectionLevelId])


GO--
ALTER TABLE [Data].[Location]
ADD	CONSTRAINT [FK_Data_Location_ContentInspectionId] FOREIGN KEY ([ContentInspectionId]) REFERENCES [Data].[ContentInspection] ([ContentInspectionId])


GO--
ALTER TABLE [Data].[MetaData]
ADD	CONSTRAINT [FK_Data_MetaData_ContentInspectionId] FOREIGN KEY ([ContentInspectionId]) REFERENCES [Data].[ContentInspection] ([ContentInspectionId])


GO--
ALTER TABLE [Data].[Organization]
ADD	CONSTRAINT [FK_Data_Organization_ContentInspectionId] FOREIGN KEY ([ContentInspectionId]) REFERENCES [Data].[ContentInspection] ([ContentInspectionId])


GO--
ALTER TABLE [Data].[OrganizationMetaData]
ADD	CONSTRAINT [FK_Data_OrganizationMetaData_ContentInspectionId] FOREIGN KEY ([ContentInspectionId]) REFERENCES [Data].[ContentInspection] ([ContentInspectionId])

,	CONSTRAINT [FK_Data_OrganizationMetaData_OrganizationId] FOREIGN KEY ([OrganizationId]) REFERENCES [Data].[Organization] ([OrganizationId])

,	CONSTRAINT [FK_Data_OrganizationMetaData_MetaDataId] FOREIGN KEY ([MetaDataId]) REFERENCES [Data].[MetaData] ([MetaDataId])


GO--
ALTER TABLE [Data].[Voter]
ADD	CONSTRAINT [FK_Data_Voter_UserId] FOREIGN KEY ([UserId]) REFERENCES [Auth].[User] ([UserId])

,	CONSTRAINT [FK_Data_Voter_ContentInspectionId] FOREIGN KEY ([ContentInspectionId]) REFERENCES [Data].[ContentInspection] ([ContentInspectionId])

,	CONSTRAINT [FK_Data_Voter_LocationId] FOREIGN KEY ([LocationId]) REFERENCES [Data].[Location] ([LocationId])

,	CONSTRAINT [FK_Data_Voter_FavoriteOrganizationId] FOREIGN KEY ([FavoriteOrganizationId]) REFERENCES [Data].[Organization] ([OrganizationId])


GO--
IF NOT EXISTS(SELECT * FROM [Auth].[User]) BEGIN

	INSERT INTO [Auth].[User]
			   ([UserName]
			   ,[Password]
			   ,[DisplayName]
			   ,[Email]
			   ,[AuthToken]
			   ,[UserToken]
			   ,[FailedLogins]
			   ,[IsActive])
	SELECT
				'SecurityAdmin'
			   ,HASHBYTES('MD5','$ecurity4dmiN!')
			   ,'Security Admin'
			   ,'Security@test.com'
			   ,NEWID()
			   ,NEWID()
			   ,0
			   ,1	
	UNION ALL SELECT
			   'test'
			   ,HASHBYTES('MD5','test')
			   ,'Test User'
			   ,'Test@test.com'
			   ,NEWID()
			   ,NEWID()
			   ,0
			   ,1
	UNION ALL SELECT
			   'test2'
			   ,HASHBYTES('MD5','test')
			   ,'Test User 2'
			   ,'Test2@test.com'
			   ,NEWID()
			   ,NEWID()
			   ,0
			   ,1
	UNION ALL SELECT
			   'test3'
			   ,HASHBYTES('MD5','test')
			   ,'Test User 3'
			   ,'Test3@test.com'
			   ,NEWID()
			   ,NEWID()
			   ,0
			   ,1
			   
END
GO--
IF NOT EXISTS(SELECT * FROM [Auth].[Role]) BEGIN

	INSERT INTO [Auth].[Role]
           ([Title]
           ,[Description]
           ,[IsActive]
           ,[ApplyToAnon]
           ,[ApplyToAllUsers]
           ,[PreventAddingUsers])
	SELECT
			   'Security Admins'
			   ,'A role for the security admin user.'
			   ,1
			   ,0
			   ,0
			   ,1
	UNION ALL SELECT
			   'All Users'
			   ,'A role that applies permissions to all users.'
			   ,1
			   ,0
			   ,1
			   ,1	
	UNION ALL SELECT
			   'Anonymous Users'
			   ,'A role that applies permissions to anonymous users (and all users).'
			   ,1
			   ,1
			   ,1
			   ,1
END
GO--
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Candidates_Exists' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Candidates_Exists', 'Client Candidates Exists',1);
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Candidates_Search' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Candidates_Search', 'Client Candidates Search',1);
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Candidates_SelectAll' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Candidates_SelectAll', 'Client Candidates SelectAll',1);
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Candidates_SelectBy_CandidateId' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Candidates_SelectBy_CandidateId', 'Client Candidates SelectBy CandidateId',1);
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Candidates_SelectBy_ContentInspectionId' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Candidates_SelectBy_ContentInspectionId', 'Client Candidates SelectBy ContentInspectionId',1);
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Candidates_SelectBy_OrganizationId' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Candidates_SelectBy_OrganizationId', 'Client Candidates SelectBy OrganizationId',1);
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Candidates_SelectBy_ProposedByUserId' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Candidates_SelectBy_ProposedByUserId', 'Client Candidates SelectBy ProposedByUserId',1);
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Candidates_SelectBy_ConfirmedByUserId' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Candidates_SelectBy_ConfirmedByUserId', 'Client Candidates SelectBy ConfirmedByUserId',1);
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Locations_Search' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Locations_Search', 'Client Locations Search',1);
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Locations_SelectAll' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Locations_SelectAll', 'Client Locations SelectAll',1);
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Locations_SelectBy_LocationId' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Locations_SelectBy_LocationId', 'Client Locations SelectBy LocationId',1);
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Locations_SelectBy_ContentInspectionId' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Locations_SelectBy_ContentInspectionId', 'Client Locations SelectBy ContentInspectionId',1);
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Locations_SelectBy_ProposedByUserId' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Locations_SelectBy_ProposedByUserId', 'Client Locations SelectBy ProposedByUserId',1);
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Locations_SelectBy_ConfirmedByUserId' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Locations_SelectBy_ConfirmedByUserId', 'Client Locations SelectBy ConfirmedByUserId',1);
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Organizations_SelectAll' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Organizations_SelectAll', 'Client Organizations SelectAll',1);
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Organizations_Exists' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Organizations_Exists', 'Client Organizations Exists',1);
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Organizations_Search' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Organizations_Search', 'Client Organizations Search',1);
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Organizations_SelectBy_OrganizationId' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Organizations_SelectBy_OrganizationId', 'Client Organizations SelectBy OrganizationId',1);
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Organizations_SelectBy_ContentInspectionId' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Organizations_SelectBy_ContentInspectionId', 'Client Organizations SelectBy ContentInspectionId',1);
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Organizations_SelectBy_ProposedByUserId' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Organizations_SelectBy_ProposedByUserId', 'Client Organizations SelectBy ProposedByUserId',1);
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Organizations_SelectBy_ConfirmedByUserId' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Organizations_SelectBy_ConfirmedByUserId', 'Client Organizations SelectBy ConfirmedByUserId',1);
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Users_SelectByUser_Current' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Users_SelectByUser_Current', 'Client Users SelectByUser Current',1);
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Users_SelectByUser_UpdateProfile' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Users_SelectByUser_UpdateProfile', 'Client Users SelectByUser UpdateProfile',1);
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Users_Exists' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Users_Exists', 'Client Users Exists',1);
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Users_Search' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Users_Search', 'Client Users Search',1);
IF NOT EXISTS(SELECT * FROM [Auth].[Permission] WHERE [PermissionName] = 'Client_Users_SelectAll' )
	INSERT INTO [Auth].[Permission] ([PermissionName],[Title],[IsRead]) VALUES ('Client_Users_SelectAll', 'Client Users SelectAll',1);

GO--
DECLARE @SecurityAdminRoleId int;
DECLARE @AllUsersRoleId int;
DECLARE @AnonymousRoleId int;
DECLARE @SecurityAdminUserId int;

SELECT @SecurityAdminRoleId = [RoleId] FROM [Auth].[Role] WHERE [Title] = 'Security Admins'
SELECT @AllUsersRoleId = [RoleId] FROM [Auth].[Role] WHERE [Title] = 'All Users'
SELECT @AnonymousRoleId = [RoleId] FROM [Auth].[Role] WHERE [Title] = 'Anonymous Users'
SELECT @SecurityAdminUserId = [UserId] FROM [Auth].[User] WHERE [UserName] = 'SecurityAdmin'

-- Grant manage user role associations to security admin
INSERT INTO [Auth].[RolePermission]([RoleId],[PermissionId])
	SELECT	@SecurityAdminRoleId, [PermissionId]
	FROM	[Auth].[Permission] P
	WHERE	[PermissionName] LIKE 'Auth%'
	AND		NOT EXISTS(
				SELECT * FROM [Auth].[RolePermission] RP 
				WHERE [RoleId] = @SecurityAdminRoleId and RP.[PermissionId] = P.[PermissionId]
			)

-- Grant write access to authenticated users
INSERT INTO [Auth].[RolePermission]([RoleId],[PermissionId])
	SELECT	@AllUsersRoleId, [PermissionId]
	FROM	[Auth].[Permission] P
	WHERE	[IsRead] = 0
	AND		NOT [PermissionName] LIKE 'Auth%'
	AND		NOT EXISTS(
				SELECT * FROM [Auth].[RolePermission] RP 
				WHERE [RoleId] = @AllUsersRoleId and RP.[PermissionId] = P.[PermissionId]
			)

-- Grant read access to anonymous users (and authenticated users)
INSERT INTO [Auth].[RolePermission]([RoleId],[PermissionId])
	SELECT	@AnonymousRoleId, [PermissionId]
	FROM	[Auth].[Permission] P
	WHERE	[IsRead] = 1
	AND		NOT [PermissionName] LIKE 'Auth%'
	AND		NOT EXISTS(
				SELECT * FROM [Auth].[RolePermission] RP 
				WHERE [RoleId] = @AnonymousRoleId and RP.[PermissionId] = P.[PermissionId]
			)

GO--
IF NOT EXISTS(SELECT * FROM [Data].[ElectionLevel]) BEGIN

	DECLARE @Now DateTime; SET @Now = GETDATE();
	DECLARE @LastId int;

	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[ElectionLevel_Insert] 
		@ContentInspectionId = @LastId,
		@ElectionLevelTitle = 'Canada - Federal / National Elections'


	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[ElectionLevel_Insert] 
		@ContentInspectionId = @LastId,
		@ElectionLevelTitle = 'Canada - Provincial / Territorial Elections'


	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Municipal_elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[ElectionLevel_Insert] 
		@ContentInspectionId = @LastId,
		@ElectionLevelTitle = 'Canada - Municipal Elections'
		

	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[ElectionLevel_Insert] 
		@ContentInspectionId = @LastId,
		@ElectionLevelTitle = 'Canada - Provincial Senate Nominations'

		
	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[ElectionLevel_Insert] 
		@ContentInspectionId = @LastId,
		@ElectionLevelTitle = 'Canada - City Mayors'
		
	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[ElectionLevel_Insert] 
		@ContentInspectionId = @LastId,
		@ElectionLevelTitle = 'Canada - City District Wards'
			   
END
GO--
IF NOT EXISTS(SELECT * FROM [Data].[Organization]) BEGIN

	DECLARE @Now DateTime; SET @Now = GETDATE();
	DECLARE @LastId int;

	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[Organization_Insert] 
		@ContentInspectionId = @LastId,
		@OrganizationName = 'Conservative'

		
	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[Organization_Insert] 
		@ContentInspectionId = @LastId,
		@OrganizationName = 'New Democratic'

		
	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[Organization_Insert] 
		@ContentInspectionId = @LastId,
		@OrganizationName = 'Liberal'

		
	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[Organization_Insert] 
		@ContentInspectionId = @LastId,
		@OrganizationName = 'Bloc Quebecois'

		
	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[Organization_Insert] 
		@ContentInspectionId = @LastId,
		@OrganizationName = 'Green'

		
	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[Organization_Insert] 
		@ContentInspectionId = @LastId,
		@OrganizationName = 'Independent'

		
	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[Organization_Insert] 
		@ContentInspectionId = @LastId,
		@OrganizationName = 'Chritsian Heritage'

		
	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[Organization_Insert] 
		@ContentInspectionId = @LastId,
		@OrganizationName = 'Marxist-Leninist'

		
	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[Organization_Insert] 
		@ContentInspectionId = @LastId,
		@OrganizationName = 'Libertarian'

		
	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[Organization_Insert] 
		@ContentInspectionId = @LastId,
		@OrganizationName = 'Progressive Canadian'

		
	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[Organization_Insert] 
		@ContentInspectionId = @LastId,
		@OrganizationName = 'Rhinoceros'

		
	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[Organization_Insert] 
		@ContentInspectionId = @LastId,
		@OrganizationName = 'Pirate'

		
	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[Organization_Insert] 
		@ContentInspectionId = @LastId,
		@OrganizationName = 'Communist'

		
	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[Organization_Insert] 
		@ContentInspectionId = @LastId,
		@OrganizationName = 'Canadian Action'

		
	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[Organization_Insert] 
		@ContentInspectionId = @LastId,
		@OrganizationName = 'Marijuana'

		
	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[Organization_Insert] 
		@ContentInspectionId = @LastId,
		@OrganizationName = 'Animal Alliance'

		
	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[Organization_Insert] 
		@ContentInspectionId = @LastId,
		@OrganizationName = 'Western Block'

		
	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[Organization_Insert] 
		@ContentInspectionId = @LastId,
		@OrganizationName = 'United'

		
	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[Organization_Insert] 
		@ContentInspectionId = @LastId,
		@OrganizationName = 'First Peoples Nation'






			   
END
GO--
IF NOT EXISTS(SELECT * FROM [Data].[MetaData]) BEGIN

	DECLARE @Now DateTime; SET @Now = GETDATE();
	DECLARE @LastId int;

	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://www.facebook.com'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[MetaData_Insert]
			@ContentInspectionId = @LastId,
			@MetaDataName = 'Facebook Page',
			@IsRequired = 0,
			@AppliesAtAllLevels = 1,
			@AppliesToCandidates = 1,
			@AppliesToOrganizations =1


	
	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://www.twitter.com'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[MetaData_Insert]
			@ContentInspectionId = @LastId,
			@MetaDataName = 'Twitter',
			@IsRequired = 0,
			@AppliesAtAllLevels = 1,
			@AppliesToCandidates = 1,
			@AppliesToOrganizations =1

			
	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://www.youtube.com'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[MetaData_Insert]
			@ContentInspectionId = @LastId,
			@MetaDataName = 'YouTube Channel',
			@IsRequired = 0,
			@AppliesAtAllLevels = 1,
			@AppliesToCandidates = 1,
			@AppliesToOrganizations =1

			
	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = ''
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[MetaData_Insert]
			@ContentInspectionId = @LastId,
			@MetaDataName = 'Platform Summary',
			@IsRequired = 1,
			@AppliesAtAllLevels = 1,
			@AppliesToCandidates = 1,
			@AppliesToOrganizations =1
			
	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://www.wikipedia.com'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC


	EXEC [Data].[MetaData_Insert]
			@ContentInspectionId = @LastId,
			@MetaDataName = 'Wikipedia Article',
			@IsRequired = 1,
			@AppliesAtAllLevels = 1,
			@AppliesToCandidates = 1,
			@AppliesToOrganizations =1

	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = ''
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[MetaData_Insert]
			@ContentInspectionId = @LastId,
			@MetaDataName = 'Platform Website',
			@IsRequired = 1,
			@AppliesAtAllLevels = 1,
			@AppliesToCandidates = 1,
			@AppliesToOrganizations =1
			   
END
GO--
IF NOT EXISTS(SELECT * FROM [Data].[Location]) BEGIN

	DECLARE @Now DateTime; SET @Now = GETDATE();
	DECLARE @LastId int;

	EXEC [Data].[ContentInspection_Insert]
			@IsArchived =0,
			@IsBeingProposed =0,
			@ProposedByUserId =1,
			@ConfirmedByUserId =1,
			@FalseInfoCount =0,
			@TrueInfoCount =1,
			@AdminInpsected =0,
			@DateLastChecked =@Now,
			@SourceUrl = 'http://en.wikipedia.org/wiki/Elections_in_Canada'
	SELECT TOP 1 @LastId = [ContentInspectionId] FROM [Data].[ContentInspection] ORDER BY [ContentInspectionId] DESC

	EXEC [Data].[Location_Insert] 
		@ContentInspectionId = @LastId,
		@LocationName = 'Edmonton, Alberta, Canada'


			   
END
GO--
