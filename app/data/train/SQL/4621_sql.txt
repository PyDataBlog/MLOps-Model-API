USE [tiketmaster]
GO
/****** Object:  Table [dbo].[Table_1]    Script Date: 28/08/2017 09:44:10 p.m. ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Table_1]') AND type in (N'U'))
DROP TABLE [dbo].[Table_1]
GO
/****** Object:  Table [dbo].[Alumnos]    Script Date: 28/08/2017 09:44:10 p.m. ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Alumnos]') AND type in (N'U'))
DROP TABLE [dbo].[Alumnos]
GO
USE [master]
GO
/****** Object:  Database [tiketmaster]    Script Date: 28/08/2017 09:44:10 p.m. ******/
IF  EXISTS (SELECT name FROM sys.databases WHERE name = N'tiketmaster')
DROP DATABASE [tiketmaster]
GO
/****** Object:  Database [tiketmaster]    Script Date: 28/08/2017 09:44:10 p.m. ******/
IF NOT EXISTS (SELECT name FROM sys.databases WHERE name = N'tiketmaster')
BEGIN
CREATE DATABASE [tiketmaster]
 

GO
ALTER DATABASE [tiketmaster] SET COMPATIBILITY_LEVEL = 110
GO
IF (1 = FULLTEXTSERVICEPROPERTY('IsFullTextInstalled'))
begin
EXEC [tiketmaster].[dbo].[sp_fulltext_database] @action = 'enable'
end
GO
ALTER DATABASE [tiketmaster] SET ANSI_NULL_DEFAULT OFF 
GO
ALTER DATABASE [tiketmaster] SET ANSI_NULLS OFF 
GO
ALTER DATABASE [tiketmaster] SET ANSI_PADDING OFF 
GO
ALTER DATABASE [tiketmaster] SET ANSI_WARNINGS OFF 
GO
ALTER DATABASE [tiketmaster] SET ARITHABORT OFF 
GO
ALTER DATABASE [tiketmaster] SET AUTO_CLOSE OFF 
GO
ALTER DATABASE [tiketmaster] SET AUTO_CREATE_STATISTICS ON 
GO
ALTER DATABASE [tiketmaster] SET AUTO_SHRINK OFF 
GO
ALTER DATABASE [tiketmaster] SET AUTO_UPDATE_STATISTICS ON 
GO
ALTER DATABASE [tiketmaster] SET CURSOR_CLOSE_ON_COMMIT OFF 
GO
ALTER DATABASE [tiketmaster] SET CURSOR_DEFAULT  GLOBAL 
GO
ALTER DATABASE [tiketmaster] SET CONCAT_NULL_YIELDS_NULL OFF 
GO
ALTER DATABASE [tiketmaster] SET NUMERIC_ROUNDABORT OFF 
GO
ALTER DATABASE [tiketmaster] SET QUOTED_IDENTIFIER OFF 
GO
ALTER DATABASE [tiketmaster] SET RECURSIVE_TRIGGERS OFF 
GO
ALTER DATABASE [tiketmaster] SET  DISABLE_BROKER 
GO
ALTER DATABASE [tiketmaster] SET AUTO_UPDATE_STATISTICS_ASYNC OFF 
GO
ALTER DATABASE [tiketmaster] SET DATE_CORRELATION_OPTIMIZATION OFF 
GO
ALTER DATABASE [tiketmaster] SET TRUSTWORTHY OFF 
GO
ALTER DATABASE [tiketmaster] SET ALLOW_SNAPSHOT_ISOLATION OFF 
GO
ALTER DATABASE [tiketmaster] SET PARAMETERIZATION SIMPLE 
GO
ALTER DATABASE [tiketmaster] SET READ_COMMITTED_SNAPSHOT OFF 
GO
ALTER DATABASE [tiketmaster] SET HONOR_BROKER_PRIORITY OFF 
GO
ALTER DATABASE [tiketmaster] SET RECOVERY SIMPLE 
GO
ALTER DATABASE [tiketmaster] SET  MULTI_USER 
GO
ALTER DATABASE [tiketmaster] SET PAGE_VERIFY CHECKSUM  
GO
ALTER DATABASE [tiketmaster] SET DB_CHAINING OFF 
GO
ALTER DATABASE [tiketmaster] SET FILESTREAM( NON_TRANSACTED_ACCESS = OFF ) 
GO
ALTER DATABASE [tiketmaster] SET TARGET_RECOVERY_TIME = 0 SECONDS 
GO
USE [tiketmaster]
GO
/****** Object:  Table [dbo].[Alumnos]    Script Date: 28/08/2017 09:44:10 p.m. ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Alumnos]') AND type in (N'U'))
BEGIN
CREATE TABLE [dbo].[Alumnos](
	[id] [int] NULL,
	[nombre] [char](100) NULL
) ON [PRIMARY]
END
GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[Table_1]    Script Date: 28/08/2017 09:44:10 p.m. ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Table_1]') AND type in (N'U'))
BEGIN
CREATE TABLE [dbo].[Table_1](
	[RE4R] [nchar](10) NULL
) ON [PRIMARY]
END
GO
USE [master]
GO
ALTER DATABASE [tiketmaster] SET  READ_WRITE 
GO
