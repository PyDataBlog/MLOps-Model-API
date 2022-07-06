DECLARE
	@LastUpdDt SMALLDATETIME = GETDATE(),
	@LastUpdUs NVARCHAR(50) = N'SV',
	@CouncilId INT = 1;

INSERT INTO [Material]([Name], [CouncilId], [LastUpdDt], [LastUpdUs])
SELECT N'дерево', @CouncilId, @LastUpdDt, @LastUpdUs;

INSERT INTO [Material]([Name], [CouncilId], [LastUpdDt], [LastUpdUs])
SELECT N'цемент',@CouncilId,  @LastUpdDt, @LastUpdUs;

INSERT INTO [Material]([Name], [CouncilId], [LastUpdDt], [LastUpdUs])
SELECT N'цегла', @CouncilId, @LastUpdDt, @LastUpdUs;