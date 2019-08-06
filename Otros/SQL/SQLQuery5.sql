USE [CarManagement]
GO

/****** Object:  Table [dbo].[vehicle]    Script Date: 06/02/2019 16:20:34 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[vehicle](
	[color] [smallint] NULL,
	[engineIsStarted] [smallint] NULL,
	[engineHorsePower] [smallint] NULL,
	[enrollmenId] [int] NOT NULL,
 CONSTRAINT [PK_vehicle] PRIMARY KEY CLUSTERED 
(
	[enrollmenId] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[vehicle]  WITH CHECK ADD  CONSTRAINT [FK_vehicle_enrollment] FOREIGN KEY([enrollmenId])
REFERENCES [dbo].[enrollment] ([id])
GO

ALTER TABLE [dbo].[vehicle] CHECK CONSTRAINT [FK_vehicle_enrollment]
GO


