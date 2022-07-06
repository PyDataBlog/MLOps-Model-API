function Get-xSPOGroupMember {
    [CmdletBinding()]
    param (
        [Parameter(Mandatory, ValueFromPipeline, ValueFromPipelineByPropertyName)]
        $Url,

        [Parameter(ValueFromPipeline, ValueFromPipelineByPropertyName)]
        $Title
    )
    
    begin {
    }
    
    process {
        $SPOClientContext = [Microsoft.SharePoint.Client.ClientContext]::new($Url)
        $SPOClientContext.Credentials = $SPOCredential
        if ($PSBoundParameters.ContainsKey('Title')) {
            $Groups = $SPOClientContext.Web.SiteGroups.GetByName($Title)
            $SPOClientContext.Load($Groups)
            $SPOClientContext.ExecuteQuery()
            $SPOClientContext.Dispose()
            foreach ($Group in $Groups) {
                $SPOClientContext.Load($Group)
                $SPOClientContext.ExecuteQuery()
                $SPOClientContext.Dispose()
                $MemberCollection = $Group.Users 
                $SPOClientContext.Load($MemberCollection)
                $SPOClientContext.ExecuteQuery()
                foreach ($Member in $MemberCollection) {
                    $Results = [pscustomobject]@{
                        Title = $Group.Title 
                        Members = $Member.LoginName
                    }
                    $Results
                }
            }
        }
        else {
            $GroupCollection = $SPOClientContext.Web.SiteGroups
            $SPOClientContext.Load($GroupCollection)
            $SPOClientContext.ExecuteQuery()
            $SPOClientContext.Dispose()
            foreach ($Group in $GroupCollection) {
                $SPOClientContext.Load($Group)
                $SPOClientContext.ExecuteQuery()
                $SPOClientContext.Dispose()
                $MemberCollection = $Group.Users 
                $SPOClientContext.Load($MemberCollection)
                $SPOClientContext.ExecuteQuery()
                foreach ($Member in $MemberCollection) {
                    $Results = [pscustomobject]@{
                        Title = $Group.Title 
                        Members = $Member.LoginName
                    }
                    $Results
                }
            }
        }
    }
    
    end {
    }
}