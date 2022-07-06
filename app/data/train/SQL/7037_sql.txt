CREATE PROCEDURE [dbo].[CreateANewJobTitle]
(
	@JobTitle NVARCHAR(128)
)
AS

	BEGIN TRANSACTION CreateJobTitle

		BEGIN TRY

		INSERT INTO dbo.JobTitle
		(
			[Title]
		)
		values(@JobTitle)

		-- TODO: Add AUDIT update here
		

	END TRY
	BEGIN CATCH
		  DECLARE @ErrorMessage NVARCHAR(4000);
            DECLARE @ErrorSeverity INT;
            DECLARE @ErrorState INT;
            
            SELECT @ErrorMessage = ERROR_MESSAGE(),
                   @ErrorSeverity = ERROR_SEVERITY(),
                   @ErrorState = ERROR_STATE();

			ROLLBACK TRANSACTION 

			   RAISERROR (@ErrorMessage, -- Message text
                       @ErrorSeverity, -- Severity
                       @ErrorState -- State
                      );
				
			RETURN 0;
	END CATCH

	
	COMMIT TRANSACTION CreateJobTitle

	RETURN 1;