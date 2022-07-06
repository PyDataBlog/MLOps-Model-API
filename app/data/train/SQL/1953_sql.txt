CREATE TABLE [dbo].[DevkitStates] (
    [id]          INT          IDENTITY (1, 1) NOT NULL,
    [DevkitState] VARCHAR (50) NOT NULL,
    CONSTRAINT [PK_DevkitStates] PRIMARY KEY CLUSTERED ([id] ASC) WITH (FILLFACTOR = 40),
    CONSTRAINT [UX_DevkitStates] UNIQUE NONCLUSTERED ([DevkitState] ASC) WITH (FILLFACTOR = 40)
);

