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
