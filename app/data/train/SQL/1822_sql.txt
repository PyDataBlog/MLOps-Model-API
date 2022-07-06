ALTER TABLE [dbo].[Position]
	ADD CONSTRAINT [FK_Position_Council_CouncilId]
	FOREIGN KEY (CouncilId)
	REFERENCES [Council] (Id)
