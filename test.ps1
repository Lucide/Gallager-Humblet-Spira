param(
    [Parameter(Mandatory)]
    [ValidateScript({ $_ -gt 0 })]
    [int]$Size
)

if (!(Test-Path -Path "$PSScriptRoot\_build\default\bin\ghs.cmd")) {
    rebar3 escriptize
    .\venv\Scripts\Activate.ps1
}

do {
    Write-Host -NoNewline "`r$(($i++)) validated runs 🦆"
    & '.\_build\default\bin\ghs.cmd' $Size | python "$PSScriptRoot\utils\validate.py"
}while ($LASTEXITCODE -eq 0 -and ![System.Console]::KeyAvailable)

if ($LASTEXITCODE -eq 0) {
    [System.Console]::ReadKey() | Out-Null
}
