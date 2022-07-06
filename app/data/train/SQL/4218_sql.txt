USE [grantapp]
GO

IF  EXISTS (SELECT * FROM sys.foreign_keys WHERE object_id = OBJECT_ID(N'[dbo].[FK_contact_history_approach]') AND parent_object_id = OBJECT_ID(N'[dbo].[contact_history]'))
ALTER TABLE [dbo].[contact_history] DROP CONSTRAINT [FK_contact_history_approach]
GO

IF  EXISTS (SELECT * FROM sys.foreign_keys WHERE object_id = OBJECT_ID(N'[dbo].[FK_contact_history_grant]') AND parent_object_id = OBJECT_ID(N'[dbo].[contact_history]'))
ALTER TABLE [dbo].[contact_history] DROP CONSTRAINT [FK_contact_history_grant]
GO

IF  EXISTS (SELECT * FROM sys.foreign_keys WHERE object_id = OBJECT_ID(N'[dbo].[FK_contact_history_user]') AND parent_object_id = OBJECT_ID(N'[dbo].[contact_history]'))
ALTER TABLE [dbo].[contact_history] DROP CONSTRAINT [FK_contact_history_user]
GO

USE [grantapp]
GO

/****** Object:  Table [dbo].[contact_history]    Script Date: 11/14/2013 18:01:05 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[contact_history]') AND type in (N'U'))
DROP TABLE [dbo].[contact_history]
GO

USE [grantapp]
GO

/****** Object:  Table [dbo].[contact_history]    Script Date: 11/14/2013 18:01:05 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[contact_history](
	[contact_history_id] [int] IDENTITY(1,1) NOT NULL,
	[grant_id] [int] NOT NULL,
	[initial_approach_id] [int] NULL,
	[contact_name] [nvarchar](50) NOT NULL,
	[call_date] [datetime] NOT NULL,
	[outcome] [nvarchar](max) NULL,
	[contacted_by] [nvarchar](50) NULL,
	[notes] [nvarchar](max) NULL,
 CONSTRAINT [PK_contact_history_1] PRIMARY KEY CLUSTERED 
(
	[contact_history_id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

ALTER TABLE [dbo].[contact_history]  WITH CHECK ADD  CONSTRAINT [FK_contact_history_approach] FOREIGN KEY([initial_approach_id])
REFERENCES [dbo].[approach] ([approach_id])
GO

ALTER TABLE [dbo].[contact_history] CHECK CONSTRAINT [FK_contact_history_approach]
GO

ALTER TABLE [dbo].[contact_history]  WITH CHECK ADD  CONSTRAINT [FK_contact_history_grant] FOREIGN KEY([grant_id])
REFERENCES [dbo].[grant] ([grant_id])
GO

ALTER TABLE [dbo].[contact_history] CHECK CONSTRAINT [FK_contact_history_grant]
GO

ALTER TABLE [dbo].[contact_history]  WITH CHECK ADD  CONSTRAINT [FK_contact_history_user] FOREIGN KEY([contacted_by])
REFERENCES [dbo].[user] ([username])
GO

ALTER TABLE [dbo].[contact_history] CHECK CONSTRAINT [FK_contact_history_user]
GO


