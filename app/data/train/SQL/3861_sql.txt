
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE name = N'Teams' AND type = 'U')
	DROP TABLE [dbo].[Teams]
GO

/****** Object:  Table [dbo].[Teams]    Script Date: 5/1/2016 6:33:12 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[Teams](
	[Id] [bigint] IDENTITY(1,1) NOT NULL,
	[Name] [nvarchar](256) NULL,
	[Sport] [nvarchar](256) NULL,
	[Description] [nvarchar](1024) NULL,
	[Ranking] [int] NULL,
PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

