function Set-xSPOListFolderCreation {
    [CmdletBinding()]
    param (
        [Parameter(Mandatory,ValueFromPipeline,ValueFromPipelineByPropertyName)]
        [uri]
        $Url,

        [Parameter(Mandatory,ValueFromPipeline,ValueFromPipelineByPropertyName)]
        [string]
        $Title,

        [Parameter(Mandatory)]
        [bool]
        $Allow
    )
    
    begin {
    }
    
    process {
        try {
            $SPOClientContext = [Microsoft.SharePoint.Client.ClientContext]::new($Url)
            $SPOClientContext.Credentials = $SPOCredential
            $List = $SPOClientContext.Web.Lists.GetByTitle($Title)
            $List.EnableFolderCreation = $Allow
            $List.Update()
            $SPOClientContext.ExecuteQuery()
            $SPOClientContext.Dispose()
        }
        catch {
            $_.Exception.Message
        }
    }
    
    end {
    }
}