param(
    [Parameter(Mandatory)]
    [ValidateRange('Positive')]
    [int]$Size
)

if (!(Test-Path -Path "$PSScriptRoot\_build\default\bin\ghs.cmd")) {
    rebar3 escriptize
    .\venv\Scripts\Activate.ps1
}

do {
    Write-Host -NoNewline "`r$(($i++)) validated runs 🦆"
    & '.\_build\default\bin\ghs.cmd' $Size | python "$PSScriptRoot\utils\show_graph.py"
}while ($LASTEXITCODE -eq 0 -and ![System.Console]::KeyAvailable)

if ($LASTEXITCODE -eq 0) {
    [System.Console]::ReadKey() | Out-Null
}
