$ServerName = "dev-sqlserver-v"
$DatabaseName = "vPICList_Lite1"

# Export stored procedures
Invoke-Sqlcmd -ServerInstance $ServerName -Database $DatabaseName -Query "
SELECT name, OBJECT_DEFINITION(object_id) AS definition
FROM sys.procedures WHERE type = 'P'
" | ForEach-Object {
    $_.definition | Out-File -FilePath "C:\Users\vm-admin\projects\export-vpiclite\exports\Scripts\SP_$($_.name).sql"
}

# Export functions
Invoke-Sqlcmd -ServerInstance $ServerName -Database $DatabaseName -Query "
SELECT name, OBJECT_DEFINITION(object_id) AS definition
FROM sys.objects WHERE type IN ('FN', 'IF', 'TF')
" | ForEach-Object {
    $_.definition | Out-File -FilePath "C:\Users\vm-admin\projects\export-vpiclite\exports\Scripts\FN_$($_.name).sql"
}