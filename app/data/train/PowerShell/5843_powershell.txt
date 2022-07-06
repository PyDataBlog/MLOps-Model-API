function Get-xSPOGroup {
    [CmdletBinding()]
    param (
        [Parameter(Mandatory, ValueFromPipeline, ValueFromPipelineByPropertyName)]
        $Url
    )
    
    begin {
    }
    
    process {
        try {
            $SPOClientContext = [Microsoft.SharePoint.Client.ClientContext]::new($Url)
            $SPOClientContext.Credentials = $SPOCredential
            $GroupCollection = $SPOClientContext.Web.SiteGroups
            $SPOClientContext.Load($GroupCollection)
            $SPOClientContext.ExecuteQuery()
            $SPOClientContext.Dispose()
            foreach ($Group in $GroupCollection) {
                $Group | Select-Object -Property ([Microsoft.SharePoint.Client.Group].GetProperties().Where( {$_.Propertytype -notlike "*Collection*"})).Name
            }
        }

        catch {
            $_.Exception.Message 
        }
    }
    
    end {
    }
}