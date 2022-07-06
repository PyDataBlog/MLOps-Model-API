CREATE TABLE [dbo].[hito](
	[hitoid] [bigint] IDENTITY(1,1) NOT NULL,
	[nhc] [bigint] NOT NULL,
	[icu] [bigint] NOT NULL,
	[facultativo] [nvarchar](255) NOT NULL,
	[localizacion] [int] NOT NULL,
	[hora_inicio] [datetime] NOT NULL,
	[hora_fin] [datetime] NOT NULL,
	[duracion] [int] NOT NULL,
	[servicio] [nvarchar](50) NOT NULL,
	[patienttype] [varchar](50) NULL,
 CONSTRAINT [PK_hito] PRIMARY KEY CLUSTERED 
(
	[hitoid] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]