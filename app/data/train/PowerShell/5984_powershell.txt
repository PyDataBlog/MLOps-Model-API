<#
.Synopsis
   Get one (or more!) wonderful facts about cats!
.DESCRIPTION
   What more do you want? Just read some cat facts, nerd...
.EXAMPLE - Just get one cat fact
   Get-CatFacts
.EXAMPLE - Get 10 cat facts!
   Get-CatFacts -Number 10
#>
function Get-CatFacts
{
    [CmdletBinding()]
    [Alias()]
    [OutputType([string])]
    Param
    (
        # Number of cat facts to obtain
        [Parameter(Mandatory=$false,
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true,
                   Position=0)]
        $Number
    )

    if (!$number)
    {
        $facts = Invoke-RestMethod -Uri http://catfacts-api.appspot.com/api/facts
    }
    else
    {
        $facts = Invoke-RestMethod -Uri http://catfacts-api.appspot.com/api/facts?number=$number
    }
    
    foreach ($fact in $facts.facts)
    {
        Write-Output "CATFACT: $fact"
    }

}

