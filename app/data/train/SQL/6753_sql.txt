SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[products](
	[id] [int] IDENTITY(1,1) NOT NULL,
	[title] [varchar](1000) COLLATE Latin1_General_CI_AS NULL,
	[price] [int] NULL,
	[description] [varchar](max) COLLATE Latin1_General_CI_AS NULL,
	[url] [varchar](2000) COLLATE Latin1_General_CI_AS NULL,
	[largeimg] [varchar](1000) COLLATE Latin1_General_CI_AS NULL,
	[smallimg] [varchar](1000) COLLATE Latin1_General_CI_AS NULL,
	[largeimgwidth] [int] NULL,
	[largeimgheight] [int] NULL,
	[smallimgwidth] [int] NULL,
	[smallimgheight] [int] NULL,
	[company] [int] NOT NULL,
 CONSTRAINT [PK_dbo.products] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON)
)

GO
SET IDENTITY_INSERT [dbo].[products] ON 

GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (1, N'Garage 3 Pack 40D Tights', 1299, N'Nylon / Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-3-Pack-40D-Tights;pgid=hFN4rwIbE34000kxvEznctJb0000JE-YYk-p;sid=Lzh428rn3Hl824XLWoJn0dtL5Uu_OACU6Ns=?SKU=1376583', N'Garage 3 Pack 40D Tights.jpg', N'Garage 3 Pack 40D Tights.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (2, N'Saben Says Berlin Handbag', 1397, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Saben-Says-Berlin-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000JE-YYk-p;sid=Lzh428rn3Hl824XLWoJn0dtL5Uu_OACU6Ns=?SKU=1525864', N'Saben Says Berlin Handbag.jpg', N'Saben Says Berlin Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (3, N'Womens Nylon Print Beachbag', 750, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Nylon-Print-Beachbag;pgid=hFN4rwIbE34000kxvEznctJb0000JE-YYk-p;sid=Lzh428rn3Hl824XLWoJn0dtL5Uu_OACU6Ns=?SKU=1539384', N'Womens Nylon Print Beachbag.jpg', N'Womens Nylon Print Beachbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (5, N'Garage 3 Pack 40D Tights', 1299, N'Nylon / Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-3-Pack-40D-Tights;pgid=s5tCsDPcEyc000oMoYT4bz7u0000lTjSxy77;sid=I6kuECD4UKsuEG_UVL4xGjBUo2rxUkz87_4=?SKU=1376583', N'Garage 3 Pack 40D Tights.jpg', N'Garage 3 Pack 40D Tights.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (6, N'Saben Says Berlin Handbag', 1397, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Saben-Says-Berlin-Handbag;pgid=s5tCsDPcEyc000oMoYT4bz7u0000lTjSxy77;sid=I6kuECD4UKsuEG_UVL4xGjBUo2rxUkz87_4=?SKU=1525864', N'Saben Says Berlin Handbag.jpg', N'Saben Says Berlin Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (7, N'Womens Nylon Print Beachbag', 750, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Nylon-Print-Beachbag;pgid=s5tCsDPcEyc000oMoYT4bz7u0000lTjSxy77;sid=I6kuECD4UKsuEG_UVL4xGjBUo2rxUkz87_4=?SKU=1539384', N'Womens Nylon Print Beachbag.jpg', N'Womens Nylon Print Beachbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (8, N'Garage 3 Pack 40D Tights', 1299, N'Nylon / Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-3-Pack-40D-Tights;pgid=s5tCsDPcEyc000oMoYT4bz7u0000qfaOnrRc;sid=MMl1xRiOHM93xVeiSnRqzwgiX6rIpT465D8=?SKU=1376583', N'Garage 3 Pack 40D Tights.jpg', N'Garage 3 Pack 40D Tights.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (9, N'Saben Says Berlin Handbag', 1397, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Saben-Says-Berlin-Handbag;pgid=s5tCsDPcEyc000oMoYT4bz7u0000qfaOnrRc;sid=MMl1xRiOHM93xVeiSnRqzwgiX6rIpT465D8=?SKU=1525864', N'Saben Says Berlin Handbag.jpg', N'Saben Says Berlin Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (10, N'Womens Nylon Print Beachbag', 750, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Nylon-Print-Beachbag;pgid=s5tCsDPcEyc000oMoYT4bz7u0000qfaOnrRc;sid=MMl1xRiOHM93xVeiSnRqzwgiX6rIpT465D8=?SKU=1539384', N'Womens Nylon Print Beachbag.jpg', N'Womens Nylon Print Beachbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (11, N'Women''s Patch Leather Front Pocket Handbag', 3499, N'Leather', N'http://www.thewarehouse.co.nz/red/catalog/product/Women%27s-Patch-Leather-Front-Pocket-Handbag;pgid=s5tCsDPcEyc000oMoYT4bz7u0000qfaOnrRc;sid=MMl1xRiOHM93xVeiSnRqzwgiX6rIpT465D8=?SKU=1533883', N'Women''s Patch Leather Front Pocket Handbag.jpg', N'Women''s Patch Leather Front Pocket Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (12, N'Garage Basic Rib Singlet', 900, N'Cotton/Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Basic-Rib-Singlet;pgid=s5tCsDPcEyc000oMoYT4bz7u0000qfaOnrRc;sid=MMl1xRiOHM93xVeiSnRqzwgiX6rIpT465D8=?SKU=1526841', N'Garage Basic Rib Singlet.jpg', N'Garage Basic Rib Singlet.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (13, N'Maya Mumtobe Round Neck T-Shirt', 1125, N'Cotton Elastane
Maternity Breastfeeding top', N'http://www.thewarehouse.co.nz/red/catalog/product/Maya-Mumtobe-Round-Neck-T-Shirt;pgid=s5tCsDPcEyc000oMoYT4bz7u0000qfaOnrRc;sid=MMl1xRiOHM93xVeiSnRqzwgiX6rIpT465D8=?SKU=1400587', N'Maya Mumtobe Round Neck T-Shirt.jpg', N'Maya Mumtobe Round Neck T-Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (14, N'Garage Military Coat', 5997, N'Outer: Polyester / Viscose, Lining: Polyester
Dry Clean Only.', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Military-Coat;pgid=s5tCsDPcEyc000oMoYT4bz7u0000qfaOnrRc;sid=MMl1xRiOHM93xVeiSnRqzwgiX6rIpT465D8=?SKU=1482653', N'Garage Military Coat.jpg', N'Garage Military Coat.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (15, N'Maya Scarf Waist Jean', 1997, N'Cotton/Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Maya-Scarf-Waist-Jean;pgid=s5tCsDPcEyc000oMoYT4bz7u0000qfaOnrRc;sid=MMl1xRiOHM93xVeiSnRqzwgiX6rIpT465D8=?SKU=1530513', N'Maya Scarf Waist Jean.jpg', N'Maya Scarf Waist Jean.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (16, N'Garage 3 Pack 70D Tights', 1699, N'Nylon / Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-3-Pack-70D-Tights;pgid=s5tCsDPcEyc000oMoYT4bz7u0000qfaOnrRc;sid=MMl1xRiOHM93xVeiSnRqzwgiX6rIpT465D8=?SKU=1376581', N'Garage 3 Pack 70D Tights.jpg', N'Garage 3 Pack 70D Tights.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (17, N'Womens Flowerfile Purse', 347, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Flowerfile-Purse;pgid=s5tCsDPcEyc000oMoYT4bz7u0000qfaOnrRc;sid=MMl1xRiOHM93xVeiSnRqzwgiX6rIpT465D8=?SKU=1536087', NULL, NULL, 0, 0, 0, 0, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (18, N'Active Intent Women''s Running Jacket', 797, N'Polyester
Windproof
Lightweight
MP3 Eyelet', N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Women%27s-Running-Jacket;pgid=s5tCsDPcEyc000oMoYT4bz7u0000qfaOnrRc;sid=MMl1xRiOHM93xVeiSnRqzwgiX6rIpT465D8=?SKU=1469745', N'Active Intent Women''s Running Jacket.jpg', N'Active Intent Women''s Running Jacket.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (19, N'Women''s Patch Leather Simple Shoulder Handbag', 2999, N'Leather', N'http://www.thewarehouse.co.nz/red/catalog/product/Women%27s-Patch-Leather-Simple-Shoulder-Handbag;pgid=s5tCsDPcEyc000oMoYT4bz7u0000qfaOnrRc;sid=MMl1xRiOHM93xVeiSnRqzwgiX6rIpT465D8=?SKU=1533880', N'Women''s Patch Leather Simple Shoulder Handbag.jpg', N'Women''s Patch Leather Simple Shoulder Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (20, N'Rachel Peasant Top', 2997, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Rachel-Peasant-Top;pgid=s5tCsDPcEyc000oMoYT4bz7u0000qfaOnrRc;sid=MMl1xRiOHM93xVeiSnRqzwgiX6rIpT465D8=?SKU=1529467', N'Rachel Peasant Top.jpg', N'Rachel Peasant Top.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (21, N'Hold Me Tight Paradise High Bikini Brief', 1997, N'82% Nylon/18% Elastane
The Paradise High Waist Swim Brief is designed to be worn with each of the holdmetight tankinis, giving extra control all through the waist and midriff up to the bust. Featuring 360 degrees of power mesh this brief ensures a smooth firm look under the tankinis.
Due to hygiene reasons swimwear cannot be returned.', N'http://www.thewarehouse.co.nz/red/catalog/product/Hold-Me-Tight-Paradise-High-Bikini-Brief;pgid=s5tCsDPcEyc000oMoYT4bz7u0000qfaOnrRc;sid=MMl1xRiOHM93xVeiSnRqzwgiX6rIpT465D8=?SKU=1526525', N'Hold Me Tight Paradise High Bikini Brief.jpg', N'Hold Me Tight Paradise High Bikini Brief.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (22, N'Rose & Thorne Perky Power - Daring Daisy Bra', 2299, N'Rose & Thorne Perky Power Bra - Daring Daisy
For a lift to die for, so natural looking. A hidden helper in the bottom of the cup decieves the eye and gives an amazing clevage. This bra can be converted to a cross over back.', N'http://www.thewarehouse.co.nz/red/catalog/product/Rose-%26-Thorne-Perky-Power---Daring-Daisy-Bra;pgid=s5tCsDPcEyc000oMoYT4bz7u0000qfaOnrRc;sid=MMl1xRiOHM93xVeiSnRqzwgiX6rIpT465D8=?SKU=1548180', N'Rose & Thorne Perky Power - Daring Daisy Bra.jpg', N'Rose & Thorne Perky Power - Daring Daisy Bra.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (23, N'Womens Dot Petal Scarf', 1500, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Dot-Petal-Scarf;pgid=s5tCsDPcEyc000oMoYT4bz7u0000qfaOnrRc;sid=MMl1xRiOHM93xVeiSnRqzwgiX6rIpT465D8=?SKU=1535163', N'Womens Dot Petal Scarf.jpg', N'Womens Dot Petal Scarf.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (24, N'Rachel Hunter Layering Singlet', 1999, N'Cotton/Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Rachel-Hunter-Layering-Singlet;pgid=s5tCsDPcEyc000oMoYT4bz7u0000qfaOnrRc;sid=MMl1xRiOHM93xVeiSnRqzwgiX6rIpT465D8=?SKU=1427846', N'Rachel Hunter Layering Singlet.jpg', N'Rachel Hunter Layering Singlet.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (25, N'Hold Me Tight Secret Desire Swimsuit', 3497, N'82% Nylon/18% Elastane
The Secret Desire Swimsuit is a sporty silhouette with 360 degrees of power mesh to provide a firm hold and smooth away unwanted bumps. Also features adjustable straps and a higher neckline for more coverage.
Due to hygiene reasons swimwear cannot be returned.', N'http://www.thewarehouse.co.nz/red/catalog/product/Hold-Me-Tight-Secret-Desire-Swimsuit;pgid=s5tCsDPcEyc000oMoYT4bz7u0000qfaOnrRc;sid=MMl1xRiOHM93xVeiSnRqzwgiX6rIpT465D8=?SKU=1526516', N'Hold Me Tight Secret Desire Swimsuit.jpg', N'Hold Me Tight Secret Desire Swimsuit.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (26, N'Women''s Patch Leather Large Backpack', 3999, N'Leather', N'http://www.thewarehouse.co.nz/red/catalog/product/Women%27s-Patch-Leather-Large-Backpack;pgid=s5tCsDPcEyc000oMoYT4bz7u0000qfaOnrRc;sid=MMl1xRiOHM93xVeiSnRqzwgiX6rIpT465D8=?SKU=1533882', N'Women''s Patch Leather Large Backpack.jpg', N'Women''s Patch Leather Large Backpack.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (27, N'Saben Says Argentina Handbag', 8999, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Saben-Says-Argentina-Handbag;pgid=s5tCsDPcEyc000oMoYT4bz7u0000qfaOnrRc;sid=MMl1xRiOHM93xVeiSnRqzwgiX6rIpT465D8=?SKU=1524631', N'Saben Says Argentina Handbag.jpg', N'Saben Says Argentina Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (28, N'Garage 3 Pack 40D Tights', 1299, N'Nylon / Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-3-Pack-40D-Tights;pgid=hFN4rwIbE34000kxvEznctJb0000XyglQCMq;sid=T5C5zlDm3dq4zh_KNOWmxEFKaSRtONAlkNI=?SKU=1376583', N'Garage 3 Pack 40D Tights.jpg', N'Garage 3 Pack 40D Tights.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (29, N'Saben Says Berlin Handbag', 1397, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Saben-Says-Berlin-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000XyglQCMq;sid=T5C5zlDm3dq4zh_KNOWmxEFKaSRtONAlkNI=?SKU=1525864', N'Saben Says Berlin Handbag.jpg', N'Saben Says Berlin Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (30, N'Womens Nylon Print Beachbag', 750, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Nylon-Print-Beachbag;pgid=hFN4rwIbE34000kxvEznctJb0000XyglQCMq;sid=T5C5zlDm3dq4zh_KNOWmxEFKaSRtONAlkNI=?SKU=1539384', N'Womens Nylon Print Beachbag.jpg', N'Womens Nylon Print Beachbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (31, N'Women''s Patch Leather Front Pocket Handbag', 3499, N'Leather', N'http://www.thewarehouse.co.nz/red/catalog/product/Women%27s-Patch-Leather-Front-Pocket-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000XyglQCMq;sid=T5C5zlDm3dq4zh_KNOWmxEFKaSRtONAlkNI=?SKU=1533883', N'Women''s Patch Leather Front Pocket Handbag.jpg', N'Women''s Patch Leather Front Pocket Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (32, N'Garage Basic Rib Singlet', 900, N'Cotton/Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Basic-Rib-Singlet;pgid=hFN4rwIbE34000kxvEznctJb0000XyglQCMq;sid=T5C5zlDm3dq4zh_KNOWmxEFKaSRtONAlkNI=?SKU=1526841', N'Garage Basic Rib Singlet.jpg', N'Garage Basic Rib Singlet.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (33, N'Maya Mumtobe Round Neck T-Shirt', 1125, N'Cotton Elastane
Maternity Breastfeeding top', N'http://www.thewarehouse.co.nz/red/catalog/product/Maya-Mumtobe-Round-Neck-T-Shirt;pgid=hFN4rwIbE34000kxvEznctJb0000XyglQCMq;sid=T5C5zlDm3dq4zh_KNOWmxEFKaSRtONAlkNI=?SKU=1400587', N'Maya Mumtobe Round Neck T-Shirt.jpg', N'Maya Mumtobe Round Neck T-Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (34, N'Garage Military Coat', 5997, N'Outer: Polyester / Viscose, Lining: Polyester
Dry Clean Only.', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Military-Coat;pgid=hFN4rwIbE34000kxvEznctJb0000XyglQCMq;sid=T5C5zlDm3dq4zh_KNOWmxEFKaSRtONAlkNI=?SKU=1482653', N'Garage Military Coat.jpg', N'Garage Military Coat.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (35, N'Maya Scarf Waist Jean', 1997, N'Cotton/Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Maya-Scarf-Waist-Jean;pgid=hFN4rwIbE34000kxvEznctJb0000XyglQCMq;sid=T5C5zlDm3dq4zh_KNOWmxEFKaSRtONAlkNI=?SKU=1530513', N'Maya Scarf Waist Jean.jpg', N'Maya Scarf Waist Jean.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (36, N'Garage 3 Pack 70D Tights', 1699, N'Nylon / Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-3-Pack-70D-Tights;pgid=hFN4rwIbE34000kxvEznctJb0000XyglQCMq;sid=T5C5zlDm3dq4zh_KNOWmxEFKaSRtONAlkNI=?SKU=1376581', N'Garage 3 Pack 70D Tights.jpg', N'Garage 3 Pack 70D Tights.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (37, N'Womens Flowerfile Purse', 347, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Flowerfile-Purse;pgid=hFN4rwIbE34000kxvEznctJb0000XyglQCMq;sid=T5C5zlDm3dq4zh_KNOWmxEFKaSRtONAlkNI=?SKU=1536087', NULL, NULL, 0, 0, 0, 0, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (38, N'Active Intent Women''s Running Jacket', 797, N'Polyester
Windproof
Lightweight
MP3 Eyelet', N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Women%27s-Running-Jacket;pgid=hFN4rwIbE34000kxvEznctJb0000XyglQCMq;sid=T5C5zlDm3dq4zh_KNOWmxEFKaSRtONAlkNI=?SKU=1469745', N'Active Intent Women''s Running Jacket.jpg', N'Active Intent Women''s Running Jacket.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (39, N'Women''s Patch Leather Simple Shoulder Handbag', 2999, N'Leather', N'http://www.thewarehouse.co.nz/red/catalog/product/Women%27s-Patch-Leather-Simple-Shoulder-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000XyglQCMq;sid=T5C5zlDm3dq4zh_KNOWmxEFKaSRtONAlkNI=?SKU=1533880', N'Women''s Patch Leather Simple Shoulder Handbag.jpg', N'Women''s Patch Leather Simple Shoulder Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (40, N'Rachel Peasant Top', 2997, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Rachel-Peasant-Top;pgid=hFN4rwIbE34000kxvEznctJb0000XyglQCMq;sid=T5C5zlDm3dq4zh_KNOWmxEFKaSRtONAlkNI=?SKU=1529467', N'Rachel Peasant Top.jpg', N'Rachel Peasant Top.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (41, N'Hold Me Tight Paradise High Bikini Brief', 1997, N'82% Nylon/18% Elastane
The Paradise High Waist Swim Brief is designed to be worn with each of the holdmetight tankinis, giving extra control all through the waist and midriff up to the bust. Featuring 360 degrees of power mesh this brief ensures a smooth firm look under the tankinis.
Due to hygiene reasons swimwear cannot be returned.', N'http://www.thewarehouse.co.nz/red/catalog/product/Hold-Me-Tight-Paradise-High-Bikini-Brief;pgid=hFN4rwIbE34000kxvEznctJb0000XyglQCMq;sid=T5C5zlDm3dq4zh_KNOWmxEFKaSRtONAlkNI=?SKU=1526525', N'Hold Me Tight Paradise High Bikini Brief.jpg', N'Hold Me Tight Paradise High Bikini Brief.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (42, N'Rose & Thorne Perky Power - Daring Daisy Bra', 2299, N'Rose & Thorne Perky Power Bra - Daring Daisy
For a lift to die for, so natural looking. A hidden helper in the bottom of the cup decieves the eye and gives an amazing clevage. This bra can be converted to a cross over back.', N'http://www.thewarehouse.co.nz/red/catalog/product/Rose-%26-Thorne-Perky-Power---Daring-Daisy-Bra;pgid=hFN4rwIbE34000kxvEznctJb0000XyglQCMq;sid=T5C5zlDm3dq4zh_KNOWmxEFKaSRtONAlkNI=?SKU=1548180', N'Rose & Thorne Perky Power - Daring Daisy Bra.jpg', N'Rose & Thorne Perky Power - Daring Daisy Bra.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (43, N'Womens Dot Petal Scarf', 1500, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Dot-Petal-Scarf;pgid=hFN4rwIbE34000kxvEznctJb0000XyglQCMq;sid=T5C5zlDm3dq4zh_KNOWmxEFKaSRtONAlkNI=?SKU=1535163', N'Womens Dot Petal Scarf.jpg', N'Womens Dot Petal Scarf.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (44, N'Rachel Hunter Layering Singlet', 1999, N'Cotton/Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Rachel-Hunter-Layering-Singlet;pgid=hFN4rwIbE34000kxvEznctJb0000XyglQCMq;sid=T5C5zlDm3dq4zh_KNOWmxEFKaSRtONAlkNI=?SKU=1427846', N'Rachel Hunter Layering Singlet.jpg', N'Rachel Hunter Layering Singlet.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (45, N'Hold Me Tight Secret Desire Swimsuit', 3497, N'82% Nylon/18% Elastane
The Secret Desire Swimsuit is a sporty silhouette with 360 degrees of power mesh to provide a firm hold and smooth away unwanted bumps. Also features adjustable straps and a higher neckline for more coverage.
Due to hygiene reasons swimwear cannot be returned.', N'http://www.thewarehouse.co.nz/red/catalog/product/Hold-Me-Tight-Secret-Desire-Swimsuit;pgid=hFN4rwIbE34000kxvEznctJb0000XyglQCMq;sid=T5C5zlDm3dq4zh_KNOWmxEFKaSRtONAlkNI=?SKU=1526516', N'Hold Me Tight Secret Desire Swimsuit.jpg', N'Hold Me Tight Secret Desire Swimsuit.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (46, N'Women''s Patch Leather Large Backpack', 3999, N'Leather', N'http://www.thewarehouse.co.nz/red/catalog/product/Women%27s-Patch-Leather-Large-Backpack;pgid=hFN4rwIbE34000kxvEznctJb0000XyglQCMq;sid=T5C5zlDm3dq4zh_KNOWmxEFKaSRtONAlkNI=?SKU=1533882', N'Women''s Patch Leather Large Backpack.jpg', N'Women''s Patch Leather Large Backpack.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (47, N'Saben Says Argentina Handbag', 8999, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Saben-Says-Argentina-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000XyglQCMq;sid=T5C5zlDm3dq4zh_KNOWmxEFKaSRtONAlkNI=?SKU=1524631', N'Saben Says Argentina Handbag.jpg', N'Saben Says Argentina Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (48, NULL, NULL, N'asdf asdf asdf ', NULL, N'image does go here', NULL, NULL, NULL, NULL, NULL, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (49, N'Garage 3 Pack 40D Tights', 1299, N'Nylon / Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-3-Pack-40D-Tights;pgid=hFN4rwIbE34000kxvEznctJb0000fNhRlgDb;sid=7uMMePppO68HeLVFbvUTcuvFbiDTOpZtIrQ=?SKU=1376583', N'Garage 3 Pack 40D Tights.jpg', N'Garage 3 Pack 40D Tights.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (50, N'Saben Says Berlin Handbag', 1397, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Saben-Says-Berlin-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000fNhRlgDb;sid=7uMMePppO68HeLVFbvUTcuvFbiDTOpZtIrQ=?SKU=1525864', N'Saben Says Berlin Handbag.jpg', N'Saben Says Berlin Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (51, N'Womens Nylon Print Beachbag', 750, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Nylon-Print-Beachbag;pgid=hFN4rwIbE34000kxvEznctJb0000fNhRlgDb;sid=7uMMePppO68HeLVFbvUTcuvFbiDTOpZtIrQ=?SKU=1539384', N'Womens Nylon Print Beachbag.jpg', N'Womens Nylon Print Beachbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (52, N'Women''s Patch Leather Front Pocket Handbag', 3499, N'Leather', N'http://www.thewarehouse.co.nz/red/catalog/product/Women%27s-Patch-Leather-Front-Pocket-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000fNhRlgDb;sid=7uMMePppO68HeLVFbvUTcuvFbiDTOpZtIrQ=?SKU=1533883', N'Women''s Patch Leather Front Pocket Handbag.jpg', N'Women''s Patch Leather Front Pocket Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (53, N'Garage Basic Rib Singlet', 900, N'Cotton/Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Basic-Rib-Singlet;pgid=hFN4rwIbE34000kxvEznctJb0000fNhRlgDb;sid=7uMMePppO68HeLVFbvUTcuvFbiDTOpZtIrQ=?SKU=1526841', N'Garage Basic Rib Singlet.jpg', N'Garage Basic Rib Singlet.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (54, N'Maya Mumtobe Round Neck T-Shirt', 1125, N'Cotton Elastane
Maternity Breastfeeding top', N'http://www.thewarehouse.co.nz/red/catalog/product/Maya-Mumtobe-Round-Neck-T-Shirt;pgid=hFN4rwIbE34000kxvEznctJb0000fNhRlgDb;sid=7uMMePppO68HeLVFbvUTcuvFbiDTOpZtIrQ=?SKU=1400587', N'Maya Mumtobe Round Neck T-Shirt.jpg', N'Maya Mumtobe Round Neck T-Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (55, N'Garage Military Coat', 5997, N'Outer: Polyester / Viscose, Lining: Polyester
Dry Clean Only.', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Military-Coat;pgid=hFN4rwIbE34000kxvEznctJb0000fNhRlgDb;sid=7uMMePppO68HeLVFbvUTcuvFbiDTOpZtIrQ=?SKU=1482653', N'Garage Military Coat.jpg', N'Garage Military Coat.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (56, N'Maya Scarf Waist Jean', 1997, N'Cotton/Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Maya-Scarf-Waist-Jean;pgid=hFN4rwIbE34000kxvEznctJb0000fNhRlgDb;sid=7uMMePppO68HeLVFbvUTcuvFbiDTOpZtIrQ=?SKU=1530513', N'Maya Scarf Waist Jean.jpg', N'Maya Scarf Waist Jean.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (57, N'Garage 3 Pack 70D Tights', 1699, N'Nylon / Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-3-Pack-70D-Tights;pgid=hFN4rwIbE34000kxvEznctJb0000fNhRlgDb;sid=7uMMePppO68HeLVFbvUTcuvFbiDTOpZtIrQ=?SKU=1376581', N'Garage 3 Pack 70D Tights.jpg', N'Garage 3 Pack 70D Tights.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (58, N'Womens Flowerfile Purse', 347, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Flowerfile-Purse;pgid=hFN4rwIbE34000kxvEznctJb0000fNhRlgDb;sid=7uMMePppO68HeLVFbvUTcuvFbiDTOpZtIrQ=?SKU=1536087', NULL, NULL, 0, 0, 0, 0, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (59, N'Active Intent Women''s Running Jacket', 797, N'Polyester
Windproof
Lightweight
MP3 Eyelet', N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Women%27s-Running-Jacket;pgid=hFN4rwIbE34000kxvEznctJb0000fNhRlgDb;sid=7uMMePppO68HeLVFbvUTcuvFbiDTOpZtIrQ=?SKU=1469745', N'Active Intent Women''s Running Jacket.jpg', N'Active Intent Women''s Running Jacket.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (60, N'Women''s Patch Leather Simple Shoulder Handbag', 2999, N'Leather', N'http://www.thewarehouse.co.nz/red/catalog/product/Women%27s-Patch-Leather-Simple-Shoulder-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000fNhRlgDb;sid=7uMMePppO68HeLVFbvUTcuvFbiDTOpZtIrQ=?SKU=1533880', N'Women''s Patch Leather Simple Shoulder Handbag.jpg', N'Women''s Patch Leather Simple Shoulder Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (61, N'Rachel Peasant Top', 2997, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Rachel-Peasant-Top;pgid=hFN4rwIbE34000kxvEznctJb0000fNhRlgDb;sid=7uMMePppO68HeLVFbvUTcuvFbiDTOpZtIrQ=?SKU=1529467', N'Rachel Peasant Top.jpg', N'Rachel Peasant Top.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (62, N'Hold Me Tight Paradise High Bikini Brief', 1997, N'82% Nylon/18% Elastane
The Paradise High Waist Swim Brief is designed to be worn with each of the holdmetight tankinis, giving extra control all through the waist and midriff up to the bust. Featuring 360 degrees of power mesh this brief ensures a smooth firm look under the tankinis.
Due to hygiene reasons swimwear cannot be returned.', N'http://www.thewarehouse.co.nz/red/catalog/product/Hold-Me-Tight-Paradise-High-Bikini-Brief;pgid=hFN4rwIbE34000kxvEznctJb0000fNhRlgDb;sid=7uMMePppO68HeLVFbvUTcuvFbiDTOpZtIrQ=?SKU=1526525', N'Hold Me Tight Paradise High Bikini Brief.jpg', N'Hold Me Tight Paradise High Bikini Brief.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (63, N'Rose & Thorne Perky Power - Daring Daisy Bra', 2299, N'Rose & Thorne Perky Power Bra - Daring Daisy
For a lift to die for, so natural looking. A hidden helper in the bottom of the cup decieves the eye and gives an amazing clevage. This bra can be converted to a cross over back.', N'http://www.thewarehouse.co.nz/red/catalog/product/Rose-%26-Thorne-Perky-Power---Daring-Daisy-Bra;pgid=hFN4rwIbE34000kxvEznctJb0000fNhRlgDb;sid=7uMMePppO68HeLVFbvUTcuvFbiDTOpZtIrQ=?SKU=1548180', N'Rose & Thorne Perky Power - Daring Daisy Bra.jpg', N'Rose & Thorne Perky Power - Daring Daisy Bra.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (64, N'Womens Dot Petal Scarf', 1500, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Dot-Petal-Scarf;pgid=hFN4rwIbE34000kxvEznctJb0000fNhRlgDb;sid=7uMMePppO68HeLVFbvUTcuvFbiDTOpZtIrQ=?SKU=1535163', N'Womens Dot Petal Scarf.jpg', N'Womens Dot Petal Scarf.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (65, N'Rachel Hunter Layering Singlet', 1999, N'Cotton/Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Rachel-Hunter-Layering-Singlet;pgid=hFN4rwIbE34000kxvEznctJb0000fNhRlgDb;sid=7uMMePppO68HeLVFbvUTcuvFbiDTOpZtIrQ=?SKU=1427846', N'Rachel Hunter Layering Singlet.jpg', N'Rachel Hunter Layering Singlet.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (66, N'Hold Me Tight Secret Desire Swimsuit', 3497, N'82% Nylon/18% Elastane
The Secret Desire Swimsuit is a sporty silhouette with 360 degrees of power mesh to provide a firm hold and smooth away unwanted bumps. Also features adjustable straps and a higher neckline for more coverage.
Due to hygiene reasons swimwear cannot be returned.', N'http://www.thewarehouse.co.nz/red/catalog/product/Hold-Me-Tight-Secret-Desire-Swimsuit;pgid=hFN4rwIbE34000kxvEznctJb0000fNhRlgDb;sid=7uMMePppO68HeLVFbvUTcuvFbiDTOpZtIrQ=?SKU=1526516', N'Hold Me Tight Secret Desire Swimsuit.jpg', N'Hold Me Tight Secret Desire Swimsuit.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (67, N'Women''s Patch Leather Large Backpack', 3999, N'Leather', N'http://www.thewarehouse.co.nz/red/catalog/product/Women%27s-Patch-Leather-Large-Backpack;pgid=hFN4rwIbE34000kxvEznctJb0000fNhRlgDb;sid=7uMMePppO68HeLVFbvUTcuvFbiDTOpZtIrQ=?SKU=1533882', N'Women''s Patch Leather Large Backpack.jpg', N'Women''s Patch Leather Large Backpack.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (68, N'Saben Says Argentina Handbag', 8999, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Saben-Says-Argentina-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000fNhRlgDb;sid=7uMMePppO68HeLVFbvUTcuvFbiDTOpZtIrQ=?SKU=1524631', N'Saben Says Argentina Handbag.jpg', N'Saben Says Argentina Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (69, N'Garage 3 Pack 40D Tights', 1299, N'Nylon / Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-3-Pack-40D-Tights;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1376583', N'Garage 3 Pack 40D Tights.jpg', N'Garage 3 Pack 40D Tights.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (70, N'Saben Says Berlin Handbag', 1397, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Saben-Says-Berlin-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1525864', N'Saben Says Berlin Handbag.jpg', N'Saben Says Berlin Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (71, N'Womens Nylon Print Beachbag', 750, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Nylon-Print-Beachbag;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1539384', N'Womens Nylon Print Beachbag.jpg', N'Womens Nylon Print Beachbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (72, N'Women''s Patch Leather Front Pocket Handbag', 3499, N'Leather', N'http://www.thewarehouse.co.nz/red/catalog/product/Women%27s-Patch-Leather-Front-Pocket-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1533883', N'Women''s Patch Leather Front Pocket Handbag.jpg', N'Women''s Patch Leather Front Pocket Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (73, N'Garage Basic Rib Singlet', 900, N'Cotton/Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Basic-Rib-Singlet;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1526841', N'Garage Basic Rib Singlet.jpg', N'Garage Basic Rib Singlet.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (74, N'Maya Mumtobe Round Neck T-Shirt', 1125, N'Cotton Elastane
Maternity Breastfeeding top', N'http://www.thewarehouse.co.nz/red/catalog/product/Maya-Mumtobe-Round-Neck-T-Shirt;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1400587', N'Maya Mumtobe Round Neck T-Shirt.jpg', N'Maya Mumtobe Round Neck T-Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (75, N'Garage Military Coat', 5997, N'Outer: Polyester / Viscose, Lining: Polyester
Dry Clean Only.', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Military-Coat;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1482653', N'Garage Military Coat.jpg', N'Garage Military Coat.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (76, N'Maya Scarf Waist Jean', 1997, N'Cotton/Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Maya-Scarf-Waist-Jean;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1530513', N'Maya Scarf Waist Jean.jpg', N'Maya Scarf Waist Jean.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (77, N'Garage 3 Pack 70D Tights', 1699, N'Nylon / Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-3-Pack-70D-Tights;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1376581', N'Garage 3 Pack 70D Tights.jpg', N'Garage 3 Pack 70D Tights.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (78, N'Womens Flowerfile Purse', 347, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Flowerfile-Purse;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1536087', NULL, NULL, 0, 0, 0, 0, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (79, N'Active Intent Women''s Running Jacket', 797, N'Polyester
Windproof
Lightweight
MP3 Eyelet', N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Women%27s-Running-Jacket;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1469745', N'Active Intent Women''s Running Jacket.jpg', N'Active Intent Women''s Running Jacket.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (80, N'Women''s Patch Leather Simple Shoulder Handbag', 2999, N'Leather', N'http://www.thewarehouse.co.nz/red/catalog/product/Women%27s-Patch-Leather-Simple-Shoulder-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1533880', N'Women''s Patch Leather Simple Shoulder Handbag.jpg', N'Women''s Patch Leather Simple Shoulder Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (81, N'Rachel Peasant Top', 2997, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Rachel-Peasant-Top;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1529467', N'Rachel Peasant Top.jpg', N'Rachel Peasant Top.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (82, N'Hold Me Tight Paradise High Bikini Brief', 1997, N'82% Nylon/18% Elastane
The Paradise High Waist Swim Brief is designed to be worn with each of the holdmetight tankinis, giving extra control all through the waist and midriff up to the bust. Featuring 360 degrees of power mesh this brief ensures a smooth firm look under the tankinis.
Due to hygiene reasons swimwear cannot be returned.', N'http://www.thewarehouse.co.nz/red/catalog/product/Hold-Me-Tight-Paradise-High-Bikini-Brief;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1526525', N'Hold Me Tight Paradise High Bikini Brief.jpg', N'Hold Me Tight Paradise High Bikini Brief.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (83, N'Rose & Thorne Perky Power - Daring Daisy Bra', 2299, N'Rose & Thorne Perky Power Bra - Daring Daisy
For a lift to die for, so natural looking. A hidden helper in the bottom of the cup decieves the eye and gives an amazing clevage. This bra can be converted to a cross over back.', N'http://www.thewarehouse.co.nz/red/catalog/product/Rose-%26-Thorne-Perky-Power---Daring-Daisy-Bra;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1548180', N'Rose & Thorne Perky Power - Daring Daisy Bra.jpg', N'Rose & Thorne Perky Power - Daring Daisy Bra.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (84, N'Womens Dot Petal Scarf', 1500, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Dot-Petal-Scarf;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1535163', N'Womens Dot Petal Scarf.jpg', N'Womens Dot Petal Scarf.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (85, N'Rachel Hunter Layering Singlet', 1999, N'Cotton/Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Rachel-Hunter-Layering-Singlet;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1427846', N'Rachel Hunter Layering Singlet.jpg', N'Rachel Hunter Layering Singlet.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (86, N'Hold Me Tight Secret Desire Swimsuit', 3497, N'82% Nylon/18% Elastane
The Secret Desire Swimsuit is a sporty silhouette with 360 degrees of power mesh to provide a firm hold and smooth away unwanted bumps. Also features adjustable straps and a higher neckline for more coverage.
Due to hygiene reasons swimwear cannot be returned.', N'http://www.thewarehouse.co.nz/red/catalog/product/Hold-Me-Tight-Secret-Desire-Swimsuit;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1526516', N'Hold Me Tight Secret Desire Swimsuit.jpg', N'Hold Me Tight Secret Desire Swimsuit.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (87, N'Women''s Patch Leather Large Backpack', 3999, N'Leather', N'http://www.thewarehouse.co.nz/red/catalog/product/Women%27s-Patch-Leather-Large-Backpack;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1533882', N'Women''s Patch Leather Large Backpack.jpg', N'Women''s Patch Leather Large Backpack.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (88, N'Saben Says Argentina Handbag', 8999, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Saben-Says-Argentina-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1524631', N'Saben Says Argentina Handbag.jpg', N'Saben Says Argentina Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (89, N'Bonds Women''s Hipster Bikini Briefs', 824, N'
Due to hygiene reasons underwear cannot be returned', N'http://www.thewarehouse.co.nz/red/catalog/product/Bonds-Women%27s-Hipster-Bikini-Briefs;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1377475', N'Bonds Women''s Hipster Bikini Briefs.jpg', N'Bonds Women''s Hipster Bikini Briefs.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (90, N'Womens Embroidered Sarong', 900, N'Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Embroidered-Sarong;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1538040', N'Womens Embroidered Sarong.jpg', N'Womens Embroidered Sarong.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (91, N'Leather Shoppers Tote Handbag', 697, N'Cow Leather with PVC trim outer, polyester lining
Fashionable large leather and PVC tote. 40cm wide x 30cm high x 15cm deep. Main zip closure, two large external front pockets, and an internal cell phone pocket and zippered pocket. Double shoulder straps measure 60cm.', N'http://www.thewarehouse.co.nz/red/catalog/product/Leather-Shoppers-Tote-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1360453', N'Leather Shoppers Tote Handbag.jpg', N'Leather Shoppers Tote Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (92, N'Maya 2 Print Dress', 2497, N'Cotton/Metallic thread', N'http://www.thewarehouse.co.nz/red/catalog/product/Maya-2-Print-Dress;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1538195', N'Maya 2 Print Dress.jpg', N'Maya 2 Print Dress.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (93, N'Maya Rib V-Neck Jumper', 1497, N'Acrylic', N'http://www.thewarehouse.co.nz/red/catalog/product/Maya-Rib-V-Neck-Jumper;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1456040', N'Maya Rib V-Neck Jumper.jpg', N'Maya Rib V-Neck Jumper.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (94, N'Maya Mumtobe Maternity Capri Pants', 2624, N'Cotton Polyester
Comfortable fit jeans with stretch tummy band', N'http://www.thewarehouse.co.nz/red/catalog/product/Maya-Mumtobe-Maternity-Capri-Pants;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1400436', N'Maya Mumtobe Maternity Capri Pants.jpg', N'Maya Mumtobe Maternity Capri Pants.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (95, N'Womens Medium Vogue purse', 1699, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Medium-Vogue-purse;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1448329', NULL, NULL, 0, 0, 0, 0, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (96, N'Womens Jacquard Shoulder Handbag', 2900, N'PU', N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Jacquard-Shoulder-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1517413', N'Womens Jacquard Shoulder Handbag.jpg', N'Womens Jacquard Shoulder Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (97, N'Liz Purse', 347, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Liz-Purse;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1477693', N'Liz Purse.jpg', N'Liz Purse.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (98, N'Rose & Thorne Nicka - Cheeky Charlie Knickers', 1199, N'Rose & Thorne Nicka - Cheeky Charlie
When you want the best of both world, smooth but not boring, pretty but not over the top.', N'http://www.thewarehouse.co.nz/red/catalog/product/Rose-%26-Thorne-Nicka---Cheeky-Charlie-Knickers;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1548597', N'Rose & Thorne Nicka - Cheeky Charlie Knickers.jpg', N'Rose & Thorne Nicka - Cheeky Charlie Knickers.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (99, N'Maya Mumtobe Cardigan', 2497, N'Acrylic / Nylon', N'http://www.thewarehouse.co.nz/red/catalog/product/Maya-Mumtobe-Cardigan;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1480539', N'Maya Mumtobe Cardigan.jpg', N'Maya Mumtobe Cardigan.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (100, N'Womens Contrast Boa Purse', 2000, N'PU', N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Contrast-Boa-Purse;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1482380', N'Womens Contrast Boa Purse.jpg', N'Womens Contrast Boa Purse.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (101, N'Bonds Women''s Cottontail Full Brief', 1274, N'
Due to hygiene reasons underwear cannot be returned', N'http://www.thewarehouse.co.nz/red/catalog/product/Bonds-Women%27s-Cottontail-Full-Brief;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1375648', N'Bonds Women''s Cottontail Full Brief.jpg', N'Bonds Women''s Cottontail Full Brief.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (102, N'Saben Says Antalya Handbag', 7999, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Saben-Says-Antalya-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1527370', N'Saben Says Antalya Handbag.jpg', N'Saben Says Antalya Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (103, N'Womens Stripes Sarong', 1000, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Stripes-Sarong;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1538037', N'Womens Stripes Sarong.jpg', N'Womens Stripes Sarong.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (104, N'Active Intent Women''s Basic Track Pant', 1000, N'Polyester Cotton
Brushed Fleece inside
Comfortable fit', N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Women%27s-Basic-Track-Pant;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1459971', N'Active Intent Women''s Basic Track Pant.jpg', N'Active Intent Women''s Basic Track Pant.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (105, N'Womens Canvas Print Handbag', 2500, N'Canvas', N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Canvas-Print-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1539387', N'Womens Canvas Print Handbag.jpg', N'Womens Canvas Print Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (106, N'Garage Truth Crop Top', 797, N'Cotton', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Truth-Crop-Top;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1512954', N'Garage Truth Crop Top.jpg', N'Garage Truth Crop Top.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (107, N'Rachel Hunter Basic Legging', 2499, N'Cotton / Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Rachel-Hunter-Basic-Legging;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1428082', N'Rachel Hunter Basic Legging.jpg', N'Rachel Hunter Basic Legging.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (108, N'Mambo Women''s Interplay Top', 997, N'Polyester/Viscose', N'http://www.thewarehouse.co.nz/red/catalog/product/Mambo-Women%27s-Interplay-Top;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1502836', NULL, NULL, 0, 0, 0, 0, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (109, N'Rose & Thorne Smooooth Operator - Perfect Polly Bra', 2299, N'Rose & Thorne Smooooth Operator - Perfect Polly
A creamy smooth perfect T shirt bra with contrasting details only you need know about - unless of course you want to show off your straps. This bra can be converted to a cross over back.', N'http://www.thewarehouse.co.nz/red/catalog/product/Rose-%26-Thorne-Smooooth-Operator---Perfect-Polly-Bra;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1548205', N'Rose & Thorne Smooooth Operator - Perfect Polly Bra.jpg', N'Rose & Thorne Smooooth Operator - Perfect Polly Bra.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (110, N'Garage Shirred Cover Up', 750, N'Cotton', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Shirred-Cover-Up;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1527297', N'Garage Shirred Cover Up.jpg', N'Garage Shirred Cover Up.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (111, N'Rachel Pocket Panel Singlet', 1497, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Rachel-Pocket-Panel-Singlet;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1526566', N'Rachel Pocket Panel Singlet.jpg', N'Rachel Pocket Panel Singlet.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (112, N'Womens Dynasty Sarong', 1000, N'Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Dynasty-Sarong;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1538042', N'Womens Dynasty Sarong.jpg', N'Womens Dynasty Sarong.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (113, N'Leather 3 Zip Cross Body Handbag', 347, N'Cow Leather with PVC trim outer, polyester lining
Small but functional multi-pocket cross body leather bag with PVC trims. Attractive panel designs to both front and back.
Two zippered main compartments, two smaller zippered pockets at the front and a convenient cell phone sized section in between.
Measures 20cm x 23cm x 6cm. Adjustable shoulder strap measures 135cm at full length.', N'http://www.thewarehouse.co.nz/red/catalog/product/Leather-3-Zip-Cross-Body-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1293595', N'Leather 3 Zip Cross Body Handbag.jpg', N'Leather 3 Zip Cross Body Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (114, N'Rose & Thorne Justa Bra - Krazy Kate Bra', 1999, N'Rose & Thorne Just A Bra - Krazy Kate
Pretty, feminine, light and frivolous yet providing reliable support. A must have.', N'http://www.thewarehouse.co.nz/red/catalog/product/Rose-%26-Thorne-Justa-Bra---Krazy-Kate-Bra;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1549189', N'Rose & Thorne Justa Bra - Krazy Kate Bra.jpg', N'Rose & Thorne Justa Bra - Krazy Kate Bra.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (115, N'Saben Says Mustique Handbag', 4999, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Saben-Says-Mustique-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1531810', NULL, NULL, 0, 0, 0, 0, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (116, N'Garage Shirred 70s Top', 1197, N'Cotton', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Shirred-70s-Top;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1531152', N'Garage Shirred 70s Top.jpg', N'Garage Shirred 70s Top.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (117, N'Keira Purse', 1499, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Keira-Purse;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1279677', N'Keira Purse.jpg', N'Keira Purse.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (118, N'Active Intent Women''s V-Neck T-Shirt', 750, N'Cotton / Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Women%27s-V-Neck-T-Shirt;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1464199', NULL, NULL, 0, 0, 0, 0, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (119, N'Active Intent Women''s Microfibre Pants', 1250, N'Polyester
Peached face, comfortable fit', N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Women%27s-Microfibre-Pants;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1460883', N'Active Intent Women''s Microfibre Pants.jpg', N'Active Intent Women''s Microfibre Pants.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (120, N'Bonds Women Hipster Boyleg Basic 10-16', 1049, N'Bonds original fit with a fresh new look
Due to hygiene reasons underwear cannot be returned', N'http://www.thewarehouse.co.nz/red/catalog/product/Bonds-Women-Hipster-Boyleg-Basic-10-16;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1386479', N'Bonds Women Hipster Boyleg Basic 10-16.jpg', N'Bonds Women Hipster Boyleg Basic 10-16.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (121, N'Mambo Women''s Leggings', 1497, N'Cotton/Polyester/Elastane
Metal Zip at hem', N'http://www.thewarehouse.co.nz/red/catalog/product/Mambo-Women%27s-Leggings;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1502768', N'Mambo Women''s Leggings.jpg', N'Mambo Women''s Leggings.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (122, N'Rachel Hunter V-Neck T-Shirt', 1497, N'Viscose', N'http://www.thewarehouse.co.nz/red/catalog/product/Rachel-Hunter-V-Neck-T-Shirt;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1427813', N'Rachel Hunter V-Neck T-Shirt.jpg', N'Rachel Hunter V-Neck T-Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (123, N'Active Intent Women''s Basic Singlet', 750, N'Cotton Elastane
Stretch Cotton singlet with piping', N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Women%27s-Basic-Singlet;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1464172', N'Active Intent Women''s Basic Singlet.jpg', N'Active Intent Women''s Basic Singlet.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (124, N'Women''s Bucket Hat', 400, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Women%27s-Bucket-Hat;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1419220', N'Women''s Bucket Hat.jpg', N'Women''s Bucket Hat.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (125, N'Women''s Fringe Scarf', 207, N'Viscose
Wear as a scarf or wrap.
Scarf measures 65cm wide by 180cm long. Several colours to chose from.', N'http://www.thewarehouse.co.nz/red/catalog/product/Women%27s-Fringe-Scarf;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1470315', N'Women''s Fringe Scarf.jpg', N'Women''s Fringe Scarf.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (126, N'Garage Cut Out Top', 1497, N'Polyester/Elastane
Laser Cut Detail', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Cut-Out-Top;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1513241', N'Garage Cut Out Top.jpg', N'Garage Cut Out Top.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (127, N'Rose & Thorne Double D''Licious - Heavenly Heidi Bra', 2499, N'Rose & Thorne Double D''Licious - Heavenly Heidi
Everyday luxury, simple, smart, sexy. Superb support for the sexy sophisticate in you.', N'http://www.thewarehouse.co.nz/red/catalog/product/Rose-%26-Thorne-Double-D%27Licious---Heavenly-Heidi-Bra;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1548421', N'Rose & Thorne Double D''Licious - Heavenly Heidi Bra.jpg', N'Rose & Thorne Double D''Licious - Heavenly Heidi Bra.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (128, N'h&h 3 Pack Knee-Hi Stockings 15 denier', 399, N'Nylon / Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/h%26h-3-Pack-Knee-Hi-Stockings-15-denier;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1376631', N'h&h 3 Pack Knee-Hi Stockings 15 denier.jpg', N'h&h 3 Pack Knee-Hi Stockings 15 denier.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (129, N'Saben Says Sahara Handbag', 8999, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Saben-Says-Sahara-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1527366', N'Saben Says Sahara Handbag.jpg', N'Saben Says Sahara Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (130, N'Womens Patch Leather Twin Strap Handbag', 2999, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Patch-Leather-Twin-Strap-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1533346', NULL, NULL, 0, 0, 0, 0, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (131, N'Mambo Women''s Laplaya Dress', 1497, N'Polyester/Cotton', N'http://www.thewarehouse.co.nz/red/catalog/product/Mambo-Women%27s-Laplaya-Dress;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1502841', N'Mambo Women''s Laplaya Dress.jpg', N'Mambo Women''s Laplaya Dress.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (132, N'Womens Chic Top Handle Handbag', 2000, N'PU', N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Chic-Top-Handle-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1536793', N'Womens Chic Top Handle Handbag.jpg', N'Womens Chic Top Handle Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (133, N'French Connection Women''s Canvas Bag', 3999, N'This genuine French Connection canvas bag is fabulous for any occasion! Sold exclusively at The Warehouse.', N'http://www.thewarehouse.co.nz/red/catalog/product/French-Connection-Women%27s-Canvas-Bag;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1404031', N'French Connection Women''s Canvas Bag.jpg', N'French Connection Women''s Canvas Bag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (134, N'Garage Frayed Denim Short', 1498, N'Cotton/Polyester/Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage--Frayed-Denim-Short;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1534885', N'Garage Frayed Denim Short.jpg', N'Garage Frayed Denim Short.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (135, N'Rose & Thorne Easy Peezy - Fabulous Faith Bra', 2299, N'Rose & Thorne Easy Peezy - Fabulous Faith
Luxurious lace, so soft and sculpting in all the right places.', N'http://www.thewarehouse.co.nz/red/catalog/product/Rose-%26-Thorne-Easy-Peezy---Fabulous-Faith-Bra;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1548399', N'Rose & Thorne Easy Peezy - Fabulous Faith Bra.jpg', N'Rose & Thorne Easy Peezy - Fabulous Faith Bra.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (136, N'Womens Yoko Purse', 1999, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Yoko-Purse;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1535877', N'Womens Yoko Purse.jpg', N'Womens Yoko Purse.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (137, N'Womens China Straw Trilby', 347, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-China-Straw-Trilby;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1519430', NULL, NULL, 0, 0, 0, 0, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (138, N'Womens Ribbon Foldup Hat', 347, N'Shell: Paper, Trim: Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Ribbon-Foldup-Hat;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1526319', N'Womens Ribbon Foldup Hat.jpg', N'Womens Ribbon Foldup Hat.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (139, N'Womens Jacquard Hobo', 2999, N'PU', N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Jacquard-Hobo;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1517414', N'Womens Jacquard Hobo.jpg', N'Womens Jacquard Hobo.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (140, N'Garage Short Sleeve Basic Cardi', 1498, N'Cotton', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Short-Sleeve-Basic-Cardi;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1532149', N'Garage Short Sleeve Basic Cardi.jpg', N'Garage Short Sleeve Basic Cardi.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (141, N'Women''s Handbag', 697, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Women%27s-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1517430', N'Women''s Handbag.jpg', N'Women''s Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (142, N'Maya Hanky Hem Print Top', 797, N'Polyester Viscose
Hanky hem top with Leaf print', N'http://www.thewarehouse.co.nz/red/catalog/product/Maya-Hanky-Hem-Print-Top;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1515461', NULL, NULL, 0, 0, 0, 0, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (143, N'Active Intent Women''s Fold Over Cropped Pants', 900, N'Cotton Elastane
Stretch Cotton capri', N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Women%27s-Fold-Over-Cropped-Pants;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1472373', N'Active Intent Women''s Fold Over Cropped Pants.jpg', N'Active Intent Women''s Fold Over Cropped Pants.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (144, N'Maya Gypsy Skirt', 2999, N'Cotton', N'http://www.thewarehouse.co.nz/red/catalog/product/Maya-Gypsy-Skirt;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1538184', N'Maya Gypsy Skirt.jpg', N'Maya Gypsy Skirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (145, N'Rose & Thorne Smooooth Operator - Blissful Betty Knickers', 999, N'Rose & Thorne Smooth Operator - Blissful Betty
A no nonsense knicker, great for everyday, work or play.', N'http://www.thewarehouse.co.nz/red/catalog/product/Rose-%26-Thorne-Smooooth-Operator---Blissful-Betty-Knickers;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1548365', N'Rose & Thorne Smooooth Operator - Blissful Betty Knickers.jpg', N'Rose & Thorne Smooooth Operator - Blissful Betty Knickers.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (146, N'Breast Cancer Foundation Singlet', 1497, N'Cotton Elastane
Singlet with front print
A donation from all â€˜Positively Pinkâ€™ products will go to The NZBCF.', N'http://www.thewarehouse.co.nz/red/catalog/product/Breast-Cancer-Foundation-Singlet;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1524709', N'Breast Cancer Foundation Singlet.jpg', N'Breast Cancer Foundation Singlet.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (147, N'Garage Basic 3/4 Length Sleeve Top', 1500, N'Cotton/Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Basic-3%252f4-Length-Sleeve-Top;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1344390', NULL, NULL, 0, 0, 0, 0, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (148, N'Active Intent Women''s Run Shorts', 750, N'Polyester
Lightweight
Draw cord on waist', N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Women%27s-Run-Shorts;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1464208', N'Active Intent Women''s Run Shorts.jpg', N'Active Intent Women''s Run Shorts.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (149, N'Womens Patch Leather Zip Crossbody Handbag', 2500, N'Leather', N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Patch-Leather-Zip-Crossbody-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1533347', N'Womens Patch Leather Zip Crossbody Handbag.jpg', N'Womens Patch Leather Zip Crossbody Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (150, N'Garage Sheer Paisley Top', 1997, N'Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Sheer-Paisley-Top;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1513317', N'Garage Sheer Paisley Top.jpg', N'Garage Sheer Paisley Top.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (151, N'Hold Me Tight St Tropez Tankini', 2997, N'82% Nylon/18% Elastane
The St Tropez Shape Tankini is a modern, stylish tankini, featuring an elegant side tie, gathered sides and a slight crossover neckline. This tankini also features 360 degrees of power mesh for a firm and smooth look, and adjustable straps. Designed to pair back with the Paradise High Waist Swim Brief.', N'http://www.thewarehouse.co.nz/red/catalog/product/Hold-Me-Tight-St-Tropez-Tankini;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1526535', N'Hold Me Tight St Tropez Tankini.jpg', N'Hold Me Tight St Tropez Tankini.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (152, N'Maya Boho Print Top', 1997, N'Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Maya-Boho-Print-Top;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1531432', N'Maya Boho Print Top.jpg', N'Maya Boho Print Top.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (153, N'Maya Mumtobe Nursing Top', 1500, N'Cotton Elastane
Maternity Breastfeeding top', N'http://www.thewarehouse.co.nz/red/catalog/product/Maya-Mumtobe-Nursing-Top;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1398801', N'Maya Mumtobe Nursing Top.jpg', N'Maya Mumtobe Nursing Top.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (154, N'Garage Crop Top', 997, N'Cotton/Polyester
Burn Out effect', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Crop-Top;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1513267', N'Garage Crop Top.jpg', N'Garage Crop Top.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (155, N'Womens Seasonprint Scarf', 800, N'Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Seasonprint-Scarf;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1519829', N'Womens Seasonprint Scarf.jpg', N'Womens Seasonprint Scarf.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (156, N'Pickaberry End-to-End Cardigan', 1997, N'Acrylic/Nylon
Long Sleeve Chunky Knitwear Cardigan', N'http://www.thewarehouse.co.nz/red/catalog/product/Pickaberry-End-to-End-Cardigan;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1372585', N'Pickaberry End-to-End Cardigan.jpg', N'Pickaberry End-to-End Cardigan.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (157, N'Womens Croc Trim Tote', 3999, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Croc-Trim-Tote;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1536796', NULL, NULL, 0, 0, 0, 0, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (158, N'Women''s Handbag', 697, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Women%27s-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1517435', N'Women''s Handbag.jpg', N'Women''s Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (159, N'Bonds Women''s Wideband HipHanger Brief', 1274, N'
Due to hygiene reasons underwear cannot be returned', N'http://www.thewarehouse.co.nz/red/catalog/product/Bonds-Women%27s-Wideband-HipHanger-Brief;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1376012', N'Bonds Women''s Wideband HipHanger Brief.jpg', N'Bonds Women''s Wideband HipHanger Brief.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (160, N'h&h 4 Pack Sheer Tights 15 denier', 699, N'Nylon / Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/h%26h-4-Pack-Sheer-Tights-15-denier;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1376636', N'h&h 4 Pack Sheer Tights 15 denier.jpg', N'h&h 4 Pack Sheer Tights 15 denier.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (161, N'Rose & Thorne Curvylicious - Elegant Eva Bra', 2499, N'Rose & Thorne Curvylicious - Elegant Eva
Too good not to show off, a fantastic shape that curves and lifts, glamorous chic.', N'http://www.thewarehouse.co.nz/red/catalog/product/Rose-%26-Thorne-Curvylicious---Elegant-Eva-Bra;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1548342', N'Rose & Thorne Curvylicious - Elegant Eva Bra.jpg', N'Rose & Thorne Curvylicious - Elegant Eva Bra.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (162, N'Kate Madison Hanky Hem Top', 997, N'Cotton', N'http://www.thewarehouse.co.nz/red/catalog/product/Kate-Madison-Hanky-Hem-Top;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1525539', N'Kate Madison Hanky Hem Top.jpg', N'Kate Madison Hanky Hem Top.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (163, N'Hold Me Tight Allure Frill Tankini', 2997, N'82% Nylon/18% Elastane
The Allure Shaping Frill Tankini has a pretty feminine look, featuring a delicate frill detail, flattering gathered sides, 360 degrees of power mesh to control, and adjustable straps. Designed to be worn with the Paradise High Waist Swim Brief.', N'http://www.thewarehouse.co.nz/red/catalog/product/Hold-Me-Tight-Allure-Frill-Tankini;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1526526', N'Hold Me Tight Allure Frill Tankini.jpg', N'Hold Me Tight Allure Frill Tankini.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (164, N'Garage Round Neck T-Shirt', 497, N'Cotton/Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Round-Neck-T-Shirt;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1340007', N'Garage Round Neck T-Shirt.jpg', N'Garage Round Neck T-Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (165, N'Women''s Sunsmart Hat', 500, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Women%27s-Sunsmart-Hat;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1416023', N'Women''s Sunsmart Hat.jpg', N'Women''s Sunsmart Hat.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (166, N'Kids Mesh Duffle Togbag', 750, N'Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Kids-Mesh-Duffle-Togbag;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1433080', N'Kids Mesh Duffle Togbag.jpg', N'Kids Mesh Duffle Togbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (167, N'Garage Long Legging', 1500, N'Viscose/Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Long-Legging;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1355416', N'Garage Long Legging.jpg', N'Garage Long Legging.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (168, N'Women''s Printed Viscose Scarf', 1500, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Women%27s-Printed-Viscose-Scarf;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1522424', N'Women''s Printed Viscose Scarf.jpg', N'Women''s Printed Viscose Scarf.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (169, N'Garage Light Layer T-Shirt', 1200, N'Cotton/Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Light-Layer-T-Shirt;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1337722', N'Garage Light Layer T-Shirt.jpg', N'Garage Light Layer T-Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (170, N'Garage Velvet Leggings', 1497, N'Polyester / Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Velvet-Leggings;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1478695', N'Garage Velvet Leggings.jpg', N'Garage Velvet Leggings.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (171, N'Womens Jacquard Tote', 3900, N'PU', N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Jacquard-Tote;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1517415', N'Womens Jacquard Tote.jpg', N'Womens Jacquard Tote.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (172, N'Maya Print Mesh Gather Top', 1875, N'Polyester/Viscose', N'http://www.thewarehouse.co.nz/red/catalog/product/Maya-Print-Mesh-Gather-Top;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1511993', N'Maya Print Mesh Gather Top.jpg', N'Maya Print Mesh Gather Top.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (173, N'Active Intent Women''s Heel Lock Sports Socks 2 Pack', 999, N'Cotton / Polyester / Nylon / Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Women%27s-Heel-Lock-Sports-Socks-2-Pack;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1466854', N'Active Intent Women''s Heel Lock Sports Socks 2 Pack.jpg', N'Active Intent Women''s Heel Lock Sports Socks 2 Pack.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (174, N'Red Stamp 5 Pack Women''s Crew Socks', 1099, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Red-Stamp-5-Pack-Women%27s-Crew-Socks;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1364258', N'Red Stamp 5 Pack Women''s Crew Socks.jpg', N'Red Stamp 5 Pack Women''s Crew Socks.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (175, N'Womens Printed Trucker', 277, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Printed-Trucker;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1526299', NULL, NULL, 0, 0, 0, 0, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (176, N'Bonds Women''s Cottontail Midi Brief', 1274, N'
Due to hygiene reasons underwear cannot be returned', N'http://www.thewarehouse.co.nz/red/catalog/product/Bonds-Women%27s-Cottontail-Midi-Brief;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1375636', N'Bonds Women''s Cottontail Midi Brief.jpg', N'Bonds Women''s Cottontail Midi Brief.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (177, N'Women''s Marty Trilby', 347, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Women%27s-Marty-Trilby;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1523708', NULL, NULL, 0, 0, 0, 0, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (178, N'Womens Wide Paper Bow Hat', 900, N'Paper', N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Wide-Paper-Bow-Hat;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1439054', N'Womens Wide Paper Bow Hat.jpg', N'Womens Wide Paper Bow Hat.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (179, N'Garage Colour Block Tank', 497, N'Cotton', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Colour-Block-Tank;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1531802', N'Garage Colour Block Tank.jpg', N'Garage Colour Block Tank.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (180, N'Active Intent Women''s Cooldry Crop Top', 1000, N'Nylon Elastane
Cooldry fabric is specially created to draw moisture away from the skin, keeping your body cool and dry during exercise.
Stretch fabric for extra comfort', N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Women%27s-Cooldry-Crop-Top;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1511685', N'Active Intent Women''s Cooldry Crop Top.jpg', N'Active Intent Women''s Cooldry Crop Top.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (181, N'Sunray Leather Shoulder Handbag', 487, N'Cow Leather with PVC trim outer, polyester lining
Classic over shoulder leather handbag with PVC trims. Panel and buckle detailing gives a fashionable look.
The bag has two compartments, each with a separate zip. One internal zippered pocket.
Measures 34cm x 19cm x 9cm. Shoulder strap measures 58cm.', N'http://www.thewarehouse.co.nz/red/catalog/product/Sunray-Leather-Shoulder-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1360450', N'Sunray Leather Shoulder Handbag.jpg', N'Sunray Leather Shoulder Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (182, N'Rachel Woven Mix Top', 1197, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Rachel-Woven-Mix-Top;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1529425', N'Rachel Woven Mix Top.jpg', N'Rachel Woven Mix Top.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (183, N'Rachel Print Kaftan Dress', 2997, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Rachel-Print-Kaftan-Dress;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1529449', N'Rachel Print Kaftan Dress.jpg', N'Rachel Print Kaftan Dress.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (184, N'Kate Madison End to End Knit Cardigan', 2999, N'Viscose', N'http://www.thewarehouse.co.nz/red/catalog/product/Kate-Madison-End-to-End-Knit-Cardigan;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1510721', N'Kate Madison End to End Knit Cardigan.jpg', N'Kate Madison End to End Knit Cardigan.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (185, N'Active Intent Women''s Cooldry Singlet', 1000, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Women%27s-Cooldry-Singlet;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1476431', N'Active Intent Women''s Cooldry Singlet.jpg', N'Active Intent Women''s Cooldry Singlet.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (186, N'Women''s Marty Trilby', 347, N'Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Women%27s-Marty-Trilby;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1526995', NULL, NULL, 0, 0, 0, 0, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (187, N'Garage Mini Dist Denim Shorts', 1998, N'Cotton/Polyester/Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Mini-Dist-Denim-Shorts;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1492928', N'Garage Mini Dist Denim Shorts.jpg', N'Garage Mini Dist Denim Shorts.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (188, N'Garage Suede Look Leggings', 1197, N'Polyester/Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage--Suede-Look-Leggings;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1513258', N'Garage Suede Look Leggings.jpg', N'Garage Suede Look Leggings.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (189, N'Kate Madison Hanky Hem Singlet', 497, N'Cotton', N'http://www.thewarehouse.co.nz/red/catalog/product/Kate-Madison-Hanky-Hem-Singlet;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1508624', N'Kate Madison Hanky Hem Singlet.jpg', N'Kate Madison Hanky Hem Singlet.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (190, N'Women''s Patch Leather Large Double Strap Handbag', 3999, N'Leather', N'http://www.thewarehouse.co.nz/red/catalog/product/Women%27s-Patch-Leather-Large-Double-Strap-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1533881', N'Women''s Patch Leather Large Double Strap Handbag.jpg', N'Women''s Patch Leather Large Double Strap Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (191, N'Garage Second Skin Jeans', 2500, N'Cotton/Polyester/Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Second-Skin-Jeans;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1444785', N'Garage Second Skin Jeans.jpg', N'Garage Second Skin Jeans.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (192, N'Women''s Handbag', 697, N'Polyurethane Outer, Cotton Lining
Measures 30 x 20 x 8cm', N'http://www.thewarehouse.co.nz/red/catalog/product/Women%27s-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1517432', N'Women''s Handbag.jpg', N'Women''s Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (193, N'Garage Box Pocket Crop Top', 497, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Box-Pocket-Crop-Top;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1530726', N'Garage Box Pocket Crop Top.jpg', N'Garage Box Pocket Crop Top.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (194, N'Garage Long Sleeve Crop Top', 497, N'Polyester/Viscose
Burn Out effect', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Long-Sleeve-Crop-Top;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1513331', N'Garage Long Sleeve Crop Top.jpg', N'Garage Long Sleeve Crop Top.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (195, N'Maya Boho Capri', 1997, N'Cotton/Polyester/Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Maya-Boho-Capri;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1534006', N'Maya Boho Capri.jpg', N'Maya Boho Capri.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (196, N'Maya Eng Capri Jean', 1875, N'Cotton/Polyester/Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Maya-Eng-Capri-Jean;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1513059', N'Maya Eng Capri Jean.jpg', N'Maya Eng Capri Jean.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (197, N'Mambo Women''s Instinct Skirt', 797, N'Viscose
Funky Mambo brand skirt with pockets.', N'http://www.thewarehouse.co.nz/red/catalog/product/Mambo-Women%27s-Instinct-Skirt;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1502773', N'Mambo Women''s Instinct Skirt.jpg', N'Mambo Women''s Instinct Skirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (198, N'Saben Says Brighton Handbag', 1397, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Saben-Says-Brighton-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1525866', N'Saben Says Brighton Handbag.jpg', N'Saben Says Brighton Handbag.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (199, N'Womens Print KD Castro', 500, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Womens-Print-KD-Castro;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1508226', N'Womens Print KD Castro.jpg', N'Womens Print KD Castro.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (200, N'Garage V Neck Cardigan', 2500, N'Acrylic/Nylon', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-V-Neck-Cardigan;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1343917', N'Garage V Neck Cardigan.jpg', N'Garage V Neck Cardigan.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (201, N'Maya Mumtobe Maternity Jeans', 2999, N'Cotton Polyester
Comfortable fit jeans with stretch tummy band', N'http://www.thewarehouse.co.nz/red/catalog/product/Maya-Mumtobe-Maternity-Jeans;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1400431', N'Maya Mumtobe Maternity Jeans.jpg', N'Maya Mumtobe Maternity Jeans.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (202, N'Rachel Zip Pocket Tee', 997, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Rachel-Zip-Pocket-Tee;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1529407', N'Rachel Zip Pocket Tee.jpg', N'Rachel Zip Pocket Tee.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (203, N'Active Intent Women''s Microfibre Capri Pants', 1000, N'Polyester
Comfortable capri fit', N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Women%27s-Microfibre-Capri-Pants;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1464663', N'Active Intent Women''s Microfibre Capri Pants.jpg', N'Active Intent Women''s Microfibre Capri Pants.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (204, N'Maya Edge to Edge Floaty Cardi', 1997, N'Acrylic', N'http://www.thewarehouse.co.nz/red/catalog/product/Maya-Edge-to-Edge-Floaty-Cardi;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1515502', NULL, NULL, 0, 0, 0, 0, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (205, N'Rachel Hunter Skinny Jeans', 1997, N'Cotton/Polyester/Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Rachel-Hunter-Skinny-Jeans;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1503158', N'Rachel Hunter Skinny Jeans.jpg', N'Rachel Hunter Skinny Jeans.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (206, N'Rose & Thorne Betty Boobs - Glorious Gloria Bra', 2299, N'Rose & Thorne Betty Boobs - Gorgeous Gloria
Great control, comfortable support. A gorgeous deluster print bra with lace top cup. Easy to wear in, all day at work, or glamorous enough to be worn out, under that little black dress!!', N'http://www.thewarehouse.co.nz/red/catalog/product/Rose-%26-Thorne-Betty-Boobs---Glorious-Gloria-Bra;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1548289', N'Rose & Thorne Betty Boobs - Glorious Gloria Bra.jpg', N'Rose & Thorne Betty Boobs - Glorious Gloria Bra.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (207, N'Garage Print Dress', 1997, N'Polyester / Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Garage-Print-Dress;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1476453', N'Garage Print Dress.jpg', N'Garage Print Dress.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (208, N'Red Stamp 5 Pack Women''s Anklet Socks', 1099, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Red-Stamp-5-Pack-Women%27s-Anklet-Socks;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1364270', N'Red Stamp 5 Pack Women''s Anklet Socks.jpg', N'Red Stamp 5 Pack Women''s Anklet Socks.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (209, N'Active Intent Women''s Fold Over Pants', 1250, N'Cotton Elastane
Stretch Cotton for extra comfort, folded waist', N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Women%27s-Fold-Over-Pants;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1460895', N'Active Intent Women''s Fold Over Pants.jpg', N'Active Intent Women''s Fold Over Pants.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (210, N'Saben Says Belize Handbag', 8999, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Saben-Says-Belize-Handbag;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1527368', NULL, NULL, 0, 0, 0, 0, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (211, N'Maya Jeans', 797, N'Cotton/Polyester/Elastane
Stretch for Comfort', N'http://www.thewarehouse.co.nz/red/catalog/product/Maya-Jeans;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1396285', N'Maya Jeans.jpg', N'Maya Jeans.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (212, N'Maya All Over Print Hanky Hem Top', 1197, N'Viscose
Hanky hem top with vintage print', N'http://www.thewarehouse.co.nz/red/catalog/product/Maya-All-Over-Print-Hanky-Hem-Top;pgid=hFN4rwIbE34000kxvEznctJb0000rLdm54RX;sid=VFWoxHd1YQasxDhZ0AS3zmbZOFFkk1HBgKM=?SKU=1511980', N'Maya All Over Print Hanky Hem Top.jpg', N'Maya All Over Print Hanky Hem Top.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (213, N'Red Stamp Men''s Basic Singlet', 590, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Red-Stamp-Men%27s-Basic-Singlet;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=249233', N'Red Stamp Men''s Basic Singlet.jpg', N'Red Stamp Men''s Basic Singlet.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (214, N'Urban Equip Straight Smash Jeans', 2497, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Urban-Equip-Straight-Smash-Jeans;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1511295', N'Urban Equip Straight Smash Jeans.jpg', N'Urban Equip Straight Smash Jeans.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (215, N'Urban Equip Wave Stripe Singlet', 797, N'Cotton, Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Urban-Equip-Wave-Stripe-Singlet;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1525523', N'Urban Equip Wave Stripe Singlet.jpg', N'Urban Equip Wave Stripe Singlet.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (216, N'h&h Mens Pyjamas', 1399, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/h%26h-Mens-Pyjamas;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1410584', N'h&h Mens Pyjamas.jpg', N'h&h Mens Pyjamas.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (217, N'Match Check Shirt', 797, N'Cotton/ Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Match-Check-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1496331', NULL, NULL, 0, 0, 0, 0, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (218, N'Bisley Long Sleeve Drill Shirt', 3799, N'2 piece collarÂ Â 
2 pleated pockets with button down flapsÂ Â 
Left pocket with pen divisionÂ Â 
2 button adjustable cuffÂ Â 
Centre back box pleatÂ 
Sun protective and Anti Static fabric
 Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Bisley-Long-Sleeve-Drill-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1381455', N'Bisley Long Sleeve Drill Shirt.jpg', N'Bisley Long Sleeve Drill Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (219, N'Urban Equip Celtic Green Boardie', 1250, N'Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Urban-Equip-Celtic-Green-Boardie;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1515728', N'Urban Equip Celtic Green Boardie.jpg', N'Urban Equip Celtic Green Boardie.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (220, N'Men''s Cotton Boxer Shorts', 1499, N'
Due to hygiene reasons underwear cannot be returned', N'http://www.thewarehouse.co.nz/red/catalog/product/Men%27s-Cotton-Boxer-Shorts;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1030233', N'Men''s Cotton Boxer Shorts.jpg', N'Men''s Cotton Boxer Shorts.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (221, N'Match Long Sleeve Shirt', 497, N'Polyester / Cotton', N'http://www.thewarehouse.co.nz/red/catalog/product/Match-Long-Sleeve-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1454364', N'Match Long Sleeve Shirt.jpg', N'Match Long Sleeve Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (222, N'Red Stamp Men''s T-Shirt', 590, N'Cotton
Handy basic for any wardrobe.', N'http://www.thewarehouse.co.nz/red/catalog/product/Red-Stamp-Men%27s-T-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=278252', N'Red Stamp Men''s T-Shirt.jpg', N'Red Stamp Men''s T-Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (223, N'Red Stamp Men''s Polo Shirt', 597, N'Polyester/Cotton', N'http://www.thewarehouse.co.nz/red/catalog/product/Red-Stamp-Men%27s-Polo-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=200875', N'Red Stamp Men''s Polo Shirt.jpg', N'Red Stamp Men''s Polo Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (224, N'Ford Men''s V8 Power T-Shirt', 797, N'Cotton', N'http://www.thewarehouse.co.nz/red/catalog/product/Ford-Men%27s-V8-Power-T-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1510054', N'Ford Men''s V8 Power T-Shirt.jpg', N'Ford Men''s V8 Power T-Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (225, N'Urban Equipment Muscle Singlet', 1050, N'Cotton
Wide shoulder design', N'http://www.thewarehouse.co.nz/red/catalog/product/Urban-Equipment-Muscle-Singlet;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=867487', N'Urban Equipment Muscle Singlet.jpg', N'Urban Equipment Muscle Singlet.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (226, N'Mens Wide Brim Straw Hat', 600, N'Straw', N'http://www.thewarehouse.co.nz/red/catalog/product/Mens-Wide-Brim-Straw-Hat;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1289180', N'Mens Wide Brim Straw Hat.jpg', N'Mens Wide Brim Straw Hat.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (227, N'Men''s Marty Trilby', 500, N'100% Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Men%27s-Marty-Trilby;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1526994', N'Men''s Marty Trilby.jpg', N'Men''s Marty Trilby.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (228, N'Men''s Ford Spectators Cap', 487, N'Cotton', N'http://www.thewarehouse.co.nz/red/catalog/product/Men%27s-Ford-Spectators-Cap;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1500925', N'Men''s Ford Spectators Cap.jpg', N'Men''s Ford Spectators Cap.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (229, N'Hanes Woven Boxers 3 Pack', 2499, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Hanes-Woven-Boxers-3-Pack;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1493586', N'Hanes Woven Boxers 3 Pack.jpg', N'Hanes Woven Boxers 3 Pack.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (230, N'Urban Equipment Straight Leg Jeans', 797, N'Cotton/Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Urban-Equipment-Straight-Leg-Jeans;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1367439', N'Urban Equipment Straight Leg Jeans.jpg', N'Urban Equipment Straight Leg Jeans.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (231, N'Bonds Men''s Guyfront Trunks', 1874, N'
Due to hygiene reasons underwear cannot be returned', N'http://www.thewarehouse.co.nz/red/catalog/product/Bonds-Men%27s-Guyfront-Trunks;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1439839', N'Bonds Men''s Guyfront Trunks.jpg', N'Bonds Men''s Guyfront Trunks.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (232, N'Men''s Cotton Boxer Shorts', 1499, N'
Due to hygiene reasons underwear cannot be returned', N'http://www.thewarehouse.co.nz/red/catalog/product/Men%27s-Cotton-Boxer-Shorts;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1030237', N'Men''s Cotton Boxer Shorts.jpg', N'Men''s Cotton Boxer Shorts.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (233, N'Red Stamp 5 Pack Men''s Business Socks', 1000, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Red-Stamp-5-Pack-Men%27s-Business-Socks;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1430505', N'Red Stamp 5 Pack Men''s Business Socks.jpg', N'Red Stamp 5 Pack Men''s Business Socks.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (234, N'Urban Equipment Basic Long Sleeve T-Shirt', 697, N'Polyester / Cotton
A must-have basic for any wardrobe.', N'http://www.thewarehouse.co.nz/red/catalog/product/Urban-Equipment-Basic-Long-Sleeve-T-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1344599', N'Urban Equipment Basic Long Sleeve T-Shirt.jpg', N'Urban Equipment Basic Long Sleeve T-Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (235, N'Active Intent Men''s Zip Off Sleeve Jacket', 2497, N'Polyester
2 in 1 Jacket and vest, zip off Sleeves reveal a mesh panelled vest.
Reflective tape on front and back. Two zip pockets', N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Men%27s-Zip-Off-Sleeve-Jacket;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1363401', N'Active Intent Men''s Zip Off Sleeve Jacket.jpg', N'Active Intent Men''s Zip Off Sleeve Jacket.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (236, N'Bonds Men''s Active Trunk', 2249, N'
Due to hygiene reasons underwear cannot be returned', N'http://www.thewarehouse.co.nz/red/catalog/product/Bonds-Men%27s-Active-Trunk;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1409892', N'Bonds Men''s Active Trunk.jpg', N'Bonds Men''s Active Trunk.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (237, N'Men''s Bi-Fold Leather Wallet', 1999, N'Cow leather, polyester lining
Men''s leather bi-fold wallet. Two note compartments.
Holds eight credit cards or ID.
Measures 11.5cm x 10cm x 1.5cm.', N'http://www.thewarehouse.co.nz/red/catalog/product/Men%27s-Bi-Fold-Leather-Wallet;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1303727', N'Men''s Bi-Fold Leather Wallet.jpg', N'Men''s Bi-Fold Leather Wallet.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (238, N'Urban Equip Boardie Cargo Basic Key Driver Charcoal/White S', 950, N'100% Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Urban-Equip-Boardie-Cargo-Basic-Key-Driver-Charcoal%252fWhite-S;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1505001', N'Urban Equip Boardie Cargo Basic Key Driver CharcoalWhite S.jpg', N'Urban Equip Boardie Cargo Basic Key Driver CharcoalWhite S.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (239, N'Men''s Mambo Cowboy Hat', 1250, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Men%27s-Mambo-Cowboy-Hat;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1535967', N'Men''s Mambo Cowboy Hat.jpg', N'Men''s Mambo Cowboy Hat.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (240, N'Red Stamp Men''s Jeans', 1150, N'Cotton/Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Red-Stamp-Men%27s-Jeans;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=554086', N'Red Stamp Men''s Jeans.jpg', N'Red Stamp Men''s Jeans.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (241, N'Mambo Men''s Rash Vest', 1749, N'Nylon Elastane
UV Protection', N'http://www.thewarehouse.co.nz/red/catalog/product/Mambo-Men%27s-Rash-Vest;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1504732', N'Mambo Men''s Rash Vest.jpg', N'Mambo Men''s Rash Vest.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (242, N'Match Stitch Detail Cargo Pant', 2297, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Match-Stitch-Detail-Cargo-Pant;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1483687', N'Match Stitch Detail Cargo Pant.jpg', N'Match Stitch Detail Cargo Pant.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (243, N'Urban Equipment Long Sleeve Rib T-Shirt', 497, N'Cotton', N'http://www.thewarehouse.co.nz/red/catalog/product/Urban-Equipment-Long-Sleeve-Rib-T-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1112104', N'Urban Equipment Long Sleeve Rib T-Shirt.jpg', N'Urban Equipment Long Sleeve Rib T-Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (244, N'Bonds 4 Pack Men''s Cotton Brief', 1874, N'
Due to hygiene reasons underwear cannot be returned', N'http://www.thewarehouse.co.nz/red/catalog/product/Bonds-4-Pack-Men%27s-Cotton-Brief;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=958368', N'Bonds 4 Pack Men''s Cotton Brief.jpg', N'Bonds 4 Pack Men''s Cotton Brief.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (245, N'Urban Equipment Printed T-Shirt', 1200, N'Cotton
Regular Fit', N'http://www.thewarehouse.co.nz/red/catalog/product/Urban-Equipment-Printed-T-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1510952', N'Urban Equipment Printed T-Shirt.jpg', N'Urban Equipment Printed T-Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (246, N'Match Mens Short Cargo Printed', 1875, N'Cotton', N'http://www.thewarehouse.co.nz/red/catalog/product/Match-Mens-Short-Cargo-Printed;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1499901', N'Match Mens Short Cargo Printed.jpg', N'Match Mens Short Cargo Printed.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (247, N'Mambo Men''s Rash Vest', 1749, N'Nylon Elastane
UV Protection', N'http://www.thewarehouse.co.nz/red/catalog/product/Mambo-Men%27s-Rash-Vest;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1504739', N'Mambo Men''s Rash Vest.jpg', N'Mambo Men''s Rash Vest.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (248, N'Men''s Holden Polo Shirt', 1997, N'Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Men%27s-Holden-Polo-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1511634', N'Men''s Holden Polo Shirt.jpg', N'Men''s Holden Polo Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (249, N'Men''s Aotearoa T-Shirt', 1500, N'Cotton
Short sleeve, crew neck t-shirt', N'http://www.thewarehouse.co.nz/red/catalog/product/Men%27s-Aotearoa-T-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1510837', N'Men''s Aotearoa T-Shirt.jpg', N'Men''s Aotearoa T-Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (250, N'Rivet Men''s Work Sock 5 Pack', 1499, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Rivet-Men%27s-Work-Sock-5-Pack;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1289592', N'Rivet Men''s Work Sock 5 Pack.jpg', N'Rivet Men''s Work Sock 5 Pack.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (251, N'Urban Equip Fine Stripe Shirt', 1597, N'Cotton (woven)', N'http://www.thewarehouse.co.nz/red/catalog/product/Urban-Equip-Fine-Stripe-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1517461', N'Urban Equip Fine Stripe Shirt.jpg', N'Urban Equip Fine Stripe Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (252, N'Mambo Sluggo Swim Pant', 999, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Mambo-Sluggo-Swim-Pant;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1529758', N'Mambo Sluggo Swim Pant.jpg', N'Mambo Sluggo Swim Pant.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (253, N'Rainy Days Mens Manual Super Mini Umbrella', 999, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Rainy-Days-Mens-Manual-Super-Mini-Umbrella;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1122382', NULL, NULL, 0, 0, 0, 0, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (254, N'Bisley Cargo Pants', 4999, N'Adjustable side tab on waistbandÂ Â 
YKK zipÂ Â 
2 angled side pocketsÂ Â 
2 side cargo pockets with touch tape flapsÂ Â 
2 back flat pockets with touch tape flapsÂ 
Sun protective and Anti Static fabric
Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Bisley-Cargo-Pants;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1381478', N'Bisley Cargo Pants.jpg', N'Bisley Cargo Pants.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (255, N'Active Intent Men''s Running Shorts', 1125, N'Nylon
Ideal outdoor short, quick to dry and lined', N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Men%27s-Running-Shorts;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1403303', N'Active Intent Men''s Running Shorts.jpg', N'Active Intent Men''s Running Shorts.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (256, N'Mambo Men''s T-Shirt', 1297, N'Nylon Elastane
UV Protection', N'http://www.thewarehouse.co.nz/red/catalog/product/Mambo-Men%27s-T-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1504744', N'Mambo Men''s T-Shirt.jpg', N'Mambo Men''s T-Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (257, N'Mambo Crop Shot Rash Vest', 1749, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Mambo-Crop-Shot-Rash-Vest;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1529715', N'Mambo Crop Shot Rash Vest.jpg', N'Mambo Crop Shot Rash Vest.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (258, N'Rivet Snow Fleece Tunic', 897, N'Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Rivet-Snow-Fleece-Tunic;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=992262', N'Rivet Snow Fleece Tunic.jpg', N'Rivet Snow Fleece Tunic.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (259, N'Rainy Days Short Golf Umbrella', 799, N'Nylon / Steel / Plastic
Two colour nylon golf umbrella. Canopy has 56cm radius.
Auto (push button) opening, steel shaft and ribs, plastic wood-look handle.', N'http://www.thewarehouse.co.nz/red/catalog/product/Rainy-Days-Short-Golf-Umbrella;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1172521', N'Rainy Days Short Golf Umbrella.jpg', N'Rainy Days Short Golf Umbrella.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (260, N'Men''s Cotton Boxer Shorts', 1124, N'
Due to hygiene reasons underwear cannot be returned', N'http://www.thewarehouse.co.nz/red/catalog/product/Men%27s-Cotton-Boxer-Shorts;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1029965', N'Men''s Cotton Boxer Shorts.jpg', N'Men''s Cotton Boxer Shorts.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (261, N'Men''s Panama Hat', 900, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Men%27s-Panama-Hat;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1529952', NULL, NULL, 0, 0, 0, 0, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (262, N'Rio 5 Pack Men''s Briefs', 2499, N'
Due to hygiene reasons underwear cannot be returned', N'http://www.thewarehouse.co.nz/red/catalog/product/Rio-5-Pack-Men%27s-Briefs;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1307278', N'Rio 5 Pack Men''s Briefs.jpg', N'Rio 5 Pack Men''s Briefs.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (263, N'Urban Equipment Baseball T-Shirt', 497, N'Combed Cotton Single Jersey', N'http://www.thewarehouse.co.nz/red/catalog/product/Urban-Equipment-Baseball-T-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1407281', N'Urban Equipment Baseball T-Shirt.jpg', N'Urban Equipment Baseball T-Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (264, N'Hanes Low Rise Briefs 4 Pack', 1999, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Hanes-Low-Rise-Briefs-4-Pack;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1493607', N'Hanes Low Rise Briefs 4 Pack.jpg', N'Hanes Low Rise Briefs 4 Pack.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (265, N'Men''s Bi-Fold Leather Wallet', 1999, N'Cow leather, polyester lining
Men''s leather bi-fold wallet. Two note compartments.
 Holds eight credit cards or ID.
Measures 11.5cm x 10cm x 1.5cm.', N'http://www.thewarehouse.co.nz/red/catalog/product/Men%27s-Bi-Fold-Leather-Wallet;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1303728', NULL, NULL, 0, 0, 0, 0, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (266, N'Urban Equipment Sherpa Sweatshirt', 2497, N'Outer: Polyester/Cotton, Inner: Polyester
Warm sherpa inner. Zip through with kanga pockets. Hood with drawcord', N'http://www.thewarehouse.co.nz/red/catalog/product/Urban-Equipment-Sherpa-Sweatshirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1355253', N'Urban Equipment Sherpa Sweatshirt.jpg', N'Urban Equipment Sherpa Sweatshirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (267, N'Dreams Crew Socks 2 Pack', 769, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Dreams-Crew-Socks-2-Pack;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1408443', N'Dreams Crew Socks 2 Pack.jpg', N'Dreams Crew Socks 2 Pack.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (268, N'Cups New Zealand Supporters Polo', 2499, N'Polyester
Designed with the New Zealand sports fan in mind, this polo has the look, feel and function of professional sporting apparel.', N'http://www.thewarehouse.co.nz/red/catalog/product/Cups-New-Zealand-Supporters-Polo;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1272534', N'Cups New Zealand Supporters Polo.jpg', N'Cups New Zealand Supporters Polo.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (269, N'Hanes Rib Vest 2 Pack', 1799, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Hanes-Rib-Vest-2-Pack;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1493584', N'Hanes Rib Vest 2 Pack.jpg', N'Hanes Rib Vest 2 Pack.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (270, N'Bisley Cargo Short', 3799, N'Adjustable side tab onÂ Â waistband
YKK zipÂ Â 
2 angled side pocketsÂ Â 
2 side welt pocketsÂ Â 
2 side cargo pockets with touch tape flapsÂ Â 
2 back welt flat pockets with touch tape flapsÂ 
Sun protective and Anti Static fabric
 Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Bisley-Cargo-Short;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1381467', N'Bisley Cargo Short.jpg', N'Bisley Cargo Short.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (271, N'Bonds Men''s Active Brief', 1574, N'
Due to hygiene reasons underwear cannot be returned', N'http://www.thewarehouse.co.nz/red/catalog/product/Bonds-Men%27s-Active-Brief;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1409883', N'Bonds Men''s Active Brief.jpg', N'Bonds Men''s Active Brief.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (272, N'Active Intent Men''s Short', 997, N'Polyester
Lined
Zip back pocket - perfect for your key', N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Men%27s-Short;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1399487', N'Active Intent Men''s Short.jpg', N'Active Intent Men''s Short.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (273, N'Cups Men''s NZ Support Mesh Polo', 2499, N'Polyester / Elastane with Polyester trim.
Designed with the New Zealand sports fan in mind, this polo has the look, feel and function of professional sporting apparel', N'http://www.thewarehouse.co.nz/red/catalog/product/Cups-Men%27s-NZ-Support-Mesh-Polo;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1499663', N'Cups Men''s NZ Support Mesh Polo.jpg', N'Cups Men''s NZ Support Mesh Polo.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (274, N'Kiwi Stockman Long Sleeve Polo Sweatshirt', 4499, N'Polyester
Great sweatshirt for outdoor activities.', N'http://www.thewarehouse.co.nz/red/catalog/product/Kiwi-Stockman-Long-Sleeve-Polo-Sweatshirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1268511', N'Kiwi Stockman Long Sleeve Polo Sweatshirt.jpg', N'Kiwi Stockman Long Sleeve Polo Sweatshirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (275, N'Active Intent Men''s Crew Sports Sock 2 Pack', 999, N'Cotton / Polyester / Nylon / Elastane', N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Men%27s-Crew-Sports-Sock-2-Pack;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1466848', N'Active Intent Men''s Crew Sports Sock 2 Pack.jpg', N'Active Intent Men''s Crew Sports Sock 2 Pack.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (276, N'Mambo Fully Lit Singlet', 2499, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Mambo-Fully-Lit-Singlet;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1529784', N'Mambo Fully Lit Singlet.jpg', N'Mambo Fully Lit Singlet.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (277, N'Active Intent Men''s Double Stripe Short', 997, N'Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Men%27s-Double-Stripe-Short;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1399734', N'Active Intent Men''s Double Stripe Short.jpg', N'Active Intent Men''s Double Stripe Short.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (278, N'Urban Equipment Regular Fit Printed T-Shirt', 797, N'Cotton', N'http://www.thewarehouse.co.nz/red/catalog/product/Urban-Equipment-Regular-Fit-Printed-T-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1510852', N'Urban Equipment Regular Fit Printed T-Shirt.jpg', N'Urban Equipment Regular Fit Printed T-Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (279, N'Rivet Snow Fleece Vest', 897, N'Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Rivet-Snow-Fleece-Vest;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=992290', N'Rivet Snow Fleece Vest.jpg', N'Rivet Snow Fleece Vest.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (280, N'Active Intent Men''s Twin Stripe Pants', 1497, N'Polyester
Fully lined with mesh lining for comfort. Has zips at the hem.', N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Men%27s-Twin-Stripe-Pants;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1437447', N'Active Intent Men''s Twin Stripe Pants.jpg', N'Active Intent Men''s Twin Stripe Pants.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (281, N'Red Stamp Gloves', 67, N'Acrylic', N'http://www.thewarehouse.co.nz/red/catalog/product/Red-Stamp-Gloves;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1344953', N'Red Stamp Gloves.jpg', N'Red Stamp Gloves.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (282, N'Jim Beam Men''s Line Up T-Shirt', 997, N'Cotton', N'http://www.thewarehouse.co.nz/red/catalog/product/Jim-Beam-Men%27s-Line-Up-T-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1510032', N'Jim Beam Men''s Line Up T-Shirt.jpg', N'Jim Beam Men''s Line Up T-Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (283, N'Rugby World Cup Country Supporters T-Shirt', 1997, N'100% Cotton
Short sleeve, crew neck t-shirt. Men''s sizing but also suitable for women.
TM Â© Rugby World Cup Limited 2008.', N'http://www.thewarehouse.co.nz/red/catalog/product/Rugby-World-Cup-Country-Supporters-T-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1504677', N'Rugby World Cup Country Supporters T-Shirt.jpg', N'Rugby World Cup Country Supporters T-Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (284, N'Rugby World Cup 2011 T-Shirt', 1997, N'100% Cotton
Short sleeve, crew neck t-shirt. Men''s sizing but also suitable for women.
TM Â© Rugby World Cup Limited 2008.', N'http://www.thewarehouse.co.nz/red/catalog/product/Rugby-World-Cup-2011-T-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1475185', N'Rugby World Cup 2011 T-Shirt.jpg', N'Rugby World Cup 2011 T-Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (285, N'Rainy Days Men''s Manual Super Mini Umbrella', 999, N'Nylon / Steel / Plastic
Manual opening small black umbrella with pouch cover.
Measures 23cm x 5cm when folded. Weighs 275g.', N'http://www.thewarehouse.co.nz/red/catalog/product/Rainy-Days-Men%27s-Manual-Super-Mini-Umbrella;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1122381', NULL, NULL, 0, 0, 0, 0, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (286, N'Men''s 1940s Trilby Hat', 750, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Men%27s-1940s-Trilby-Hat;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1534289', N'Men''s 1940s Trilby Hat.jpg', N'Men''s 1940s Trilby Hat.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (287, N'Mambo Men''s Word Up Boardshorts', 1999, N'Polyester
Mambo Branded
Dome back pocket', N'http://www.thewarehouse.co.nz/red/catalog/product/Mambo-Men%27s-Word-Up-Boardshorts;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1504727', N'Mambo Men''s Word Up Boardshorts.jpg', N'Mambo Men''s Word Up Boardshorts.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (288, N'Urban Equipment Bomber Jacket', 3997, N'Outer Shell: PU, Inner & Hood: Polyester
Fully lined. Hood with drawcord.
Rib cuffs, neck and hem. Four pockets', N'http://www.thewarehouse.co.nz/red/catalog/product/Urban-Equipment-Bomber-Jacket;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1368043', N'Urban Equipment Bomber Jacket.jpg', N'Urban Equipment Bomber Jacket.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (289, N'Kiwi Stockman Men''s Quick Dry Pants', 2497, N'Polyester
Quick dry, breathable', N'http://www.thewarehouse.co.nz/red/catalog/product/Kiwi-Stockman-Men%27s-Quick-Dry-Pants;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1503558', NULL, NULL, 0, 0, 0, 0, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (290, N'Mambo MBO Tshirt', 2499, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Mambo-MBO-Tshirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1529774', N'Mambo MBO Tshirt.jpg', N'Mambo MBO Tshirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (291, N'Red Stamp Men''s Stripe Polo Shirt', 597, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Red-Stamp-Men%27s-Stripe-Polo-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1456240', N'Red Stamp Men''s Stripe Polo Shirt.jpg', N'Red Stamp Men''s Stripe Polo Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (292, N'Haka Men''s Koru T-Shirt', 1297, N'Cotton
Haka brand
New Zealand inspired garment', N'http://www.thewarehouse.co.nz/red/catalog/product/Haka-Men%27s-Koru-T-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1505129', N'Haka Men''s Koru T-Shirt.jpg', N'Haka Men''s Koru T-Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (293, N'Bisley Short Sleeve Drill Shirt', 3499, N'2 Piece collarÂ Â 
2 pleated pockets with button down flapsÂ Â 
Left pocket with pen divisionÂ Â 
Centre back box pleatÂ 
Sun protective and Anti Static fabric
 Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Bisley-Short-Sleeve-Drill-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1381434', N'Bisley Short Sleeve Drill Shirt.jpg', N'Bisley Short Sleeve Drill Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (294, N'Men''s Haka Aotearoa Trucker Cap', 1000, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Men%27s-Haka-Aotearoa-Trucker-Cap;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1536757', N'Men''s Haka Aotearoa Trucker Cap.jpg', N'Men''s Haka Aotearoa Trucker Cap.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (295, N'Men''s Tri-Fold Leather Wallet', 1999, N'Cow leather, polyester lining
Men''s leather tri-fold wallet. One note compartments, mesh ID pocket.
Holds nine credit cards or ID.
Measures 10.5cm x 8.5cm x 1.5cm.', N'http://www.thewarehouse.co.nz/red/catalog/product/Men%27s-Tri-Fold-Leather-Wallet;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1303764', N'Men''s Tri-Fold Leather Wallet.jpg', N'Men''s Tri-Fold Leather Wallet.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (296, N'Bonds Men''s Guyfront Trunks', 1874, N'
Due to hygiene reasons underwear cannot be returned', N'http://www.thewarehouse.co.nz/red/catalog/product/Bonds-Men%27s-Guyfront-Trunks;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1438676', N'Bonds Men''s Guyfront Trunks.jpg', N'Bonds Men''s Guyfront Trunks.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (297, N'Hanes Modern Trunks 2 Pack', 2499, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Hanes-Modern-Trunks-2-Pack;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1493600', N'Hanes Modern Trunks 2 Pack.jpg', N'Hanes Modern Trunks 2 Pack.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (298, N'Match Short Sleeve Shirt Island Print', 1875, N'Viscose Rayon', N'http://www.thewarehouse.co.nz/red/catalog/product/Match-Short-Sleeve-Shirt-Island-Print;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1519180', N'Match Short Sleeve Shirt Island Print.jpg', N'Match Short Sleeve Shirt Island Print.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (299, N'Bonds Men''s Fit Trunk', 1874, N'
Due to hygiene reasons underwear cannot be returned', N'http://www.thewarehouse.co.nz/red/catalog/product/Bonds-Men%27s-Fit-Trunk;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1386596', N'Bonds Men''s Fit Trunk.jpg', N'Bonds Men''s Fit Trunk.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (300, N'Rugby World Cup T-Shirt', 1997, N'Cotton
A great way to show your support for the Rugby World Cup 2011. Men''s sizing but also suitable for women.
TM Â© Rugby World Cup Limited 2008.', N'http://www.thewarehouse.co.nz/red/catalog/product/Rugby-World-Cup-T-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1416187', N'Rugby World Cup T-Shirt.jpg', N'Rugby World Cup T-Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (301, N'Mambo The Man Tee', 2499, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Mambo-The-Man-Tee;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1529751', N'Mambo The Man Tee.jpg', N'Mambo The Man Tee.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (302, N'Active Intent Men''s Crew Neck T-Shirt', 697, N'Cotton', N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Men%27s-Crew-Neck-T-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1397667', N'Active Intent Men''s Crew Neck T-Shirt.jpg', N'Active Intent Men''s Crew Neck T-Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (303, N'Mambo Men''s Shorts', 1997, N'Cotton
Mambo Branded Metal Badge
6 pockets', N'http://www.thewarehouse.co.nz/red/catalog/product/Mambo-Men%27s-Shorts;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1504723', N'Mambo Men''s Shorts.jpg', N'Mambo Men''s Shorts.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (304, N'Red Stamp Beanie', 67, N'Acrylic', N'http://www.thewarehouse.co.nz/red/catalog/product/Red-Stamp-Beanie;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1344867', N'Red Stamp Beanie.jpg', N'Red Stamp Beanie.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (305, N'Ford Men''s Polo Shirt', 1997, N'Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Ford-Men%27s-Polo-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1510057', N'Ford Men''s Polo Shirt.jpg', N'Ford Men''s Polo Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (306, N'Men''s Nepal Wool Beanie', 347, N'Pure New Zealand Wool with a polyester lining
NZ Wool beanie, hand knitted in Nepal with cosy fleece lining. Size M suitable for most women, size L suitable for most men. A variety of colours and designs available. (See in store for kids sizes too).', N'http://www.thewarehouse.co.nz/red/catalog/product/Men%27s-Nepal-Wool-Beanie;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1348891', N'Men''s Nepal Wool Beanie.jpg', N'Men''s Nepal Wool Beanie.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (307, N'Urban Equip Fine Stripe Shirt', 1597, N'Cotton (woven)', N'http://www.thewarehouse.co.nz/red/catalog/product/Urban-Equip-Fine-Stripe-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1517455', N'Urban Equip Fine Stripe Shirt.jpg', N'Urban Equip Fine Stripe Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (308, N'Match Striped Polo Shirt', 997, N'Cotton Pique Jersey', N'http://www.thewarehouse.co.nz/red/catalog/product/Match-Striped-Polo-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1504824', N'Match Striped Polo Shirt.jpg', N'Match Striped Polo Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (309, N'Urban Equipment Skater Pants', 2625, N'Polyester/Cotton
Structured waist with zip fly and button. Five pockets', N'http://www.thewarehouse.co.nz/red/catalog/product/Urban-Equipment-Skater-Pants;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1120690', N'Urban Equipment Skater Pants.jpg', N'Urban Equipment Skater Pants.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (310, N'Match Men''s Elastic Waist Drill Cargo Shorts', 1500, N'Cotton', N'http://www.thewarehouse.co.nz/red/catalog/product/Match-Men%27s-Elastic-Waist-Drill-Cargo-Shorts;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1497927', N'Match Men''s Elastic Waist Drill Cargo Shorts.jpg', N'Match Men''s Elastic Waist Drill Cargo Shorts.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (311, N'Active Intent Men''s Plain Microfibre Shorts', 1125, N'Polyester
Comfort elastic waist, clean design', N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Men%27s-Plain-Microfibre-Shorts;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1183275', N'Active Intent Men''s Plain Microfibre Shorts.jpg', N'Active Intent Men''s Plain Microfibre Shorts.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (312, N'Urban Equipment Basic Hooded Sweatshirt', 597, N'Polyester / Cotton', N'http://www.thewarehouse.co.nz/red/catalog/product/Urban-Equipment-Basic-Hooded-Sweatshirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1233443', N'Urban Equipment Basic Hooded Sweatshirt.jpg', N'Urban Equipment Basic Hooded Sweatshirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (313, N'Active Intent Men''s Basic Pant', 1297, N'Polyester/Cotton
Comfort fit elastic waist with drawcord.
Brushed inner for extra warmth. Back pocket', N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Men%27s-Basic-Pant;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=492535', N'Active Intent Men''s Basic Pant.jpg', N'Active Intent Men''s Basic Pant.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (314, N'Active Intent Tip Peak Cap', 499, N'Polyester
One Size', N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Tip-Peak-Cap;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1294797', N'Active Intent Tip Peak Cap.jpg', N'Active Intent Tip Peak Cap.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (315, N'Men''s Red Trilby Hat', 750, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Men%27s-Red-Trilby-Hat;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1533737', N'Men''s Red Trilby Hat.jpg', N'Men''s Red Trilby Hat.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (316, N'Match Slogan T-Shirt', 497, N'Cotton', N'http://www.thewarehouse.co.nz/red/catalog/product/Match-Slogan-T-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1496838', N'Match Slogan T-Shirt.jpg', N'Match Slogan T-Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (317, N'Mambo Men''s Dog Gone T-Shirt', 1297, N'Cotton', N'http://www.thewarehouse.co.nz/red/catalog/product/Mambo-Men%27s-Dog-Gone-T-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1504760', N'Mambo Men''s Dog Gone T-Shirt.jpg', N'Mambo Men''s Dog Gone T-Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (318, N'Kiwi Stockman Men''s Check Shirt', 1297, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Kiwi-Stockman-Men%27s-Check-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1503576', N'Kiwi Stockman Men''s Check Shirt.jpg', N'Kiwi Stockman Men''s Check Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (319, N'Alpsocks Mens Work Budget Socks', 1299, N'Made in New Zealand.
Triple pack of mens work socks.', N'http://www.thewarehouse.co.nz/red/catalog/product/Alpsocks-Mens-Work-Budget-Socks;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=431231', N'Alpsocks Mens Work Budget Socks.jpg', N'Alpsocks Mens Work Budget Socks.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (320, N'Urban Equipment Jake Singlet', 1050, N'Cotton', N'http://www.thewarehouse.co.nz/red/catalog/product/Urban-Equipment-Jake-Singlet;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1506035', N'Urban Equipment Jake Singlet.jpg', N'Urban Equipment Jake Singlet.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (321, N'Urban Equipment Big Check Shirt', 1297, N'Cotton
 Yarn Dyed Woven Check', N'http://www.thewarehouse.co.nz/red/catalog/product/Urban-Equipment-Big-Check-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1507252', N'Urban Equipment Big Check Shirt.jpg', N'Urban Equipment Big Check Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (322, N'Urban Equipment Fitted Printed T-Shirt', 797, N'Cotton', N'http://www.thewarehouse.co.nz/red/catalog/product/Urban-Equipment-Fitted-Printed-T-Shirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1510961', N'Urban Equipment Fitted Printed T-Shirt.jpg', N'Urban Equipment Fitted Printed T-Shirt.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (323, N'Bisley Coveralls', 2497, N'Heavy duty nylon zip fasteningÂ 
Front zip patch pocket, pen and tool patch pocketÂ Â 
Side pockets and mobile phone side pocketÂ Â 
Back patch pocketsÂ Â 
Closed cuffÂ Â 
Double layer of fabric on kneesÂ 
Sun protective', N'http://www.thewarehouse.co.nz/red/catalog/product/Bisley-Coveralls;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1535313', N'Bisley Coveralls.jpg', N'Bisley Coveralls.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (324, N'Urban Equip Splatter Check Boardie', 1250, N'Polyester', N'http://www.thewarehouse.co.nz/red/catalog/product/Urban-Equip-Splatter-Check-Boardie;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1522369', N'Urban Equip Splatter Check Boardie.jpg', N'Urban Equip Splatter Check Boardie.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (325, N'Basic Cap', 300, N'Cotton
One Size', N'http://www.thewarehouse.co.nz/red/catalog/product/Basic-Cap;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1289150', N'Basic Cap.jpg', N'Basic Cap.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (326, N'Men''s Narrow Tie', 1299, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Men%27s-Narrow-Tie;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1292495', N'Men''s Narrow Tie.jpg', N'Men''s Narrow Tie.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (327, N'Mambo Men''s Denim Short', 4499, N'Cotton
Embroided Logo', N'http://www.thewarehouse.co.nz/red/catalog/product/Mambo-Men%27s-Denim-Short;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1504770', N'Mambo Men''s Denim Short.jpg', N'Mambo Men''s Denim Short.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (328, N'Active Intent Men''s Panelled Shorts', 497, N'Polyester
Contrast Piping. Full mesh lining.
Elastic waist with drawcord. Two pockets', N'http://www.thewarehouse.co.nz/red/catalog/product/Active-Intent-Men%27s-Panelled-Shorts;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1399771', N'Active Intent Men''s Panelled Shorts.jpg', N'Active Intent Men''s Panelled Shorts.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (329, N'Mambo Cross Dog Boardie', 1999, NULL, N'http://www.thewarehouse.co.nz/red/catalog/product/Mambo-Cross-Dog-Boardie;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1529745', N'Mambo Cross Dog Boardie.jpg', N'Mambo Cross Dog Boardie.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (330, N'Trucker Cap', 277, N'Polyester
One Size', N'http://www.thewarehouse.co.nz/red/catalog/product/Trucker-Cap;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1515079', N'Trucker Cap.jpg', N'Trucker Cap.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (331, N'Kiwi Stockmans Men''s Quick Dry Shorts', 1997, N'Polyester
Quick dry, breathable', N'http://www.thewarehouse.co.nz/red/catalog/product/Kiwi-Stockmans-Men%27s-Quick-Dry-Shorts;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=1503581', N'Kiwi Stockmans Men''s Quick Dry Shorts.jpg', N'Kiwi Stockmans Men''s Quick Dry Shorts.jpg', 450, 450, 110, 110, 0)
GO
INSERT [dbo].[products] ([id], [title], [price], [description], [url], [largeimg], [smallimg], [largeimgwidth], [largeimgheight], [smallimgwidth], [smallimgheight], [company]) VALUES (332, N'Rivet Basic Crew Neck Sweatshirt', 1400, N'Polyester / Cotton', N'http://www.thewarehouse.co.nz/red/catalog/product/Rivet-Basic-Crew-Neck-Sweatshirt;pgid=hFN4rwIbE34000kxvEznctJb00003P0uhfYs;sid=0OG2jTYNufKyjXkhV6OphyahvOV62llubYE=?SKU=992258', N'Rivet Basic Crew Neck Sweatshirt.jpg', N'Rivet Basic Crew Neck Sweatshirt.jpg', 450, 450, 110, 110, 0)
GO
SET IDENTITY_INSERT [dbo].[products] OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[cat](
	[prodid] [int] NOT NULL,
	[catid] [int] NOT NULL,
	[id] [int] IDENTITY(1,1) NOT NULL,
 CONSTRAINT [PK_dbo.cat] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON)
)

GO
SET IDENTITY_INSERT [dbo].[cat] ON 

GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (1, 2, 1)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (2, 2, 2)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (3, 2, 3)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (5, 2, 4)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (6, 2, 5)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (7, 2, 6)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (8, 2, 7)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (9, 2, 8)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (10, 2, 9)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (11, 2, 10)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (12, 2, 11)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (13, 2, 12)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (13, 6, 13)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (14, 2, 14)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (15, 2, 15)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (16, 2, 16)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (17, 2, 17)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (18, 2, 18)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (19, 2, 19)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (20, 2, 20)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (21, 2, 21)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (21, 19, 22)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (22, 2, 23)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (22, 21, 24)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (23, 2, 25)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (24, 2, 26)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (25, 2, 27)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (25, 19, 28)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (26, 2, 29)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (27, 2, 30)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (28, 2, 31)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (29, 2, 32)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (30, 2, 33)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (31, 2, 34)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (32, 2, 35)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (33, 2, 36)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (33, 6, 37)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (34, 2, 38)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (35, 2, 39)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (36, 2, 40)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (37, 2, 41)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (38, 2, 42)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (39, 2, 43)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (40, 2, 44)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (41, 2, 45)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (41, 19, 46)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (42, 2, 47)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (42, 21, 48)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (43, 2, 49)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (44, 2, 50)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (45, 2, 51)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (45, 19, 52)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (46, 2, 53)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (47, 2, 54)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (49, 2, 55)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (50, 2, 56)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (51, 2, 57)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (52, 2, 58)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (53, 2, 59)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (54, 2, 60)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (54, 6, 61)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (55, 2, 62)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (56, 2, 63)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (57, 2, 64)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (58, 2, 65)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (59, 2, 66)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (60, 2, 67)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (61, 2, 68)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (62, 2, 69)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (62, 19, 70)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (63, 2, 71)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (63, 21, 72)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (64, 2, 73)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (65, 2, 74)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (66, 2, 75)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (66, 19, 76)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (67, 2, 77)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (68, 2, 78)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (69, 2, 79)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (70, 2, 80)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (71, 2, 81)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (72, 2, 82)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (73, 2, 83)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (74, 2, 84)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (74, 6, 85)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (75, 2, 86)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (76, 2, 87)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (77, 2, 88)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (78, 2, 89)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (79, 2, 90)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (80, 2, 91)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (81, 2, 92)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (82, 2, 93)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (82, 19, 94)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (83, 2, 95)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (83, 21, 96)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (84, 2, 97)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (85, 2, 98)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (86, 2, 99)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (86, 19, 100)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (87, 2, 101)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (88, 2, 102)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (89, 2, 103)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (89, 21, 104)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (90, 2, 105)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (91, 2, 106)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (92, 2, 107)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (93, 2, 108)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (94, 2, 109)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (94, 8, 110)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (95, 2, 111)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (96, 2, 112)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (97, 2, 113)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (98, 2, 114)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (98, 6, 115)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (99, 2, 116)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (100, 2, 117)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (101, 2, 118)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (101, 21, 119)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (102, 2, 120)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (103, 2, 121)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (104, 2, 122)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (105, 2, 123)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (106, 2, 124)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (107, 2, 125)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (108, 2, 126)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (109, 2, 127)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (109, 6, 128)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (109, 21, 129)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (109, 21, 130)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (110, 2, 131)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (111, 2, 132)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (112, 2, 133)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (113, 2, 134)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (113, 15, 135)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (114, 2, 136)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (115, 2, 137)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (116, 2, 138)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (117, 2, 139)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (118, 2, 140)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (119, 2, 141)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (120, 2, 142)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (120, 21, 143)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (121, 2, 144)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (122, 2, 145)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (123, 2, 146)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (123, 6, 147)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (124, 2, 148)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (125, 2, 149)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (125, 16, 150)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (126, 2, 151)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (127, 2, 152)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (128, 2, 153)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (129, 2, 154)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (130, 2, 155)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (131, 2, 156)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (132, 2, 157)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (133, 2, 158)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (133, 15, 159)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (134, 2, 160)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (135, 2, 161)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (136, 2, 162)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (137, 2, 163)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (138, 2, 164)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (139, 2, 165)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (140, 2, 166)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (141, 2, 167)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (142, 2, 168)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (142, 6, 169)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (143, 2, 170)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (144, 2, 171)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (145, 2, 172)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (146, 2, 173)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (147, 2, 174)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (148, 2, 175)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (149, 2, 176)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (150, 2, 177)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (151, 2, 178)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (151, 19, 179)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (151, 17, 180)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (151, 19, 181)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (152, 2, 182)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (153, 2, 183)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (153, 6, 184)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (154, 2, 185)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (155, 2, 186)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (156, 2, 187)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (157, 2, 188)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (158, 2, 189)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (159, 2, 190)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (159, 21, 191)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (160, 2, 192)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (161, 2, 193)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (162, 2, 194)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (163, 2, 195)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (164, 2, 196)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (165, 2, 197)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (166, 2, 198)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (167, 2, 199)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (168, 2, 200)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (169, 2, 201)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (170, 2, 202)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (171, 2, 203)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (172, 2, 204)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (173, 2, 205)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (174, 2, 206)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (175, 2, 207)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (176, 2, 208)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (176, 21, 209)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (177, 2, 210)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (178, 2, 211)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (179, 2, 212)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (180, 2, 213)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (181, 2, 214)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (181, 15, 215)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (181, 15, 216)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (182, 2, 217)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (183, 2, 218)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (184, 2, 219)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (185, 2, 220)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (186, 2, 221)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (187, 2, 222)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (188, 2, 223)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (189, 2, 224)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (190, 2, 225)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (191, 2, 226)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (192, 2, 227)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (193, 2, 228)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (194, 2, 229)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (195, 2, 230)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (196, 2, 231)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (197, 2, 232)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (197, 10, 233)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (198, 2, 234)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (199, 2, 235)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (200, 2, 236)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (201, 2, 237)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (201, 8, 238)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (202, 2, 239)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (203, 2, 240)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (204, 2, 241)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (205, 2, 242)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (206, 2, 243)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (206, 21, 244)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (206, 6, 245)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (206, 10, 246)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (207, 2, 247)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (208, 2, 248)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (209, 2, 249)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (210, 2, 250)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (211, 2, 251)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (212, 2, 252)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (212, 6, 253)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (213, 1, 254)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (214, 1, 255)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (215, 1, 256)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (216, 1, 257)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (217, 1, 258)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (218, 1, 259)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (219, 1, 260)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (220, 1, 261)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (220, 21, 262)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (221, 1, 263)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (222, 1, 264)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (223, 1, 265)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (224, 1, 266)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (225, 1, 267)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (226, 1, 268)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (227, 1, 269)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (228, 1, 270)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (229, 1, 271)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (230, 1, 272)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (231, 1, 273)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (231, 21, 274)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (232, 1, 275)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (232, 21, 276)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (233, 1, 277)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (234, 1, 278)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (235, 1, 279)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (235, 7, 280)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (236, 1, 281)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (236, 21, 282)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (237, 1, 283)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (238, 1, 284)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (239, 1, 285)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (240, 1, 286)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (241, 1, 287)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (242, 1, 288)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (243, 1, 289)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (244, 1, 290)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (244, 21, 291)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (245, 1, 292)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (246, 1, 293)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (247, 1, 294)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (248, 1, 295)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (249, 1, 296)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (249, 6, 297)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (250, 1, 298)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (251, 1, 299)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (252, 1, 300)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (253, 1, 301)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (254, 1, 302)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (255, 1, 303)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (255, 9, 304)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (256, 1, 305)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (257, 1, 306)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (258, 1, 307)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (259, 1, 308)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (260, 1, 309)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (260, 21, 310)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (261, 1, 311)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (262, 1, 312)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (262, 21, 313)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (263, 1, 314)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (264, 1, 315)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (265, 1, 316)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (266, 1, 317)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (267, 1, 318)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (268, 1, 319)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (269, 1, 320)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (270, 1, 321)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (271, 1, 322)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (271, 21, 323)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (272, 1, 324)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (273, 1, 325)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (274, 1, 326)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (274, 7, 327)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (275, 1, 328)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (276, 1, 329)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (277, 1, 330)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (278, 1, 331)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (279, 1, 332)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (280, 1, 333)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (281, 1, 334)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (282, 1, 335)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (283, 1, 336)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (283, 6, 337)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (284, 1, 338)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (284, 6, 339)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (285, 1, 340)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (286, 1, 341)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (287, 1, 342)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (288, 1, 343)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (289, 1, 344)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (290, 1, 345)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (291, 1, 346)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (292, 1, 347)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (293, 1, 348)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (294, 1, 349)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (295, 1, 350)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (296, 1, 351)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (296, 21, 352)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (297, 1, 353)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (298, 1, 354)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (299, 1, 355)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (299, 21, 356)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (300, 1, 357)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (301, 1, 358)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (302, 1, 359)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (303, 1, 360)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (304, 1, 361)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (305, 1, 362)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (306, 1, 363)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (306, 14, 364)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (306, 7, 365)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (306, 1, 366)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (307, 1, 367)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (308, 1, 368)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (309, 1, 369)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (310, 1, 370)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (311, 1, 371)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (312, 1, 372)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (313, 1, 373)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (314, 1, 374)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (315, 1, 375)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (316, 1, 376)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (317, 1, 377)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (318, 1, 378)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (319, 1, 379)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (319, 1, 380)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (319, 11, 381)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (320, 1, 382)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (321, 1, 383)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (322, 1, 384)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (323, 1, 385)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (324, 1, 386)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (325, 1, 387)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (326, 1, 388)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (327, 1, 389)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (328, 1, 390)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (329, 1, 391)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (330, 1, 392)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (331, 1, 393)
GO
INSERT [dbo].[cat] ([prodid], [catid], [id]) VALUES (332, 1, 394)
GO
SET IDENTITY_INSERT [dbo].[cat] OFF
GO
ALTER TABLE [dbo].[cat]  WITH CHECK ADD  CONSTRAINT [product_cat] FOREIGN KEY([prodid])
REFERENCES [dbo].[products] ([id])
GO
ALTER TABLE [dbo].[cat] CHECK CONSTRAINT [product_cat]
GO
