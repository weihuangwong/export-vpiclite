# Install SqlServer module if not already installed
# Install-Module -Name SqlServer -Force

# Set connection parameters
$ServerName = "dev-sqlserver-v"
$DatabaseName = "vPICList_Lite1"
$ExportPath = "C:\Users\vm-admin\projects\export-vpiclite\exports\tables\"

# Get all table names
$tables = Invoke-Sqlcmd -ServerInstance $ServerName -Database $DatabaseName -Query "SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_TYPE = 'BASE TABLE'"

# Export each table
foreach ($table in $tables) {
    $tableName = $table.TABLE_NAME
    $query = "SELECT * FROM [$tableName]"
    $outputFile = "$ExportPath$tableName.csv"
    
    Invoke-Sqlcmd -ServerInstance $ServerName -Database $DatabaseName -Query $query | Export-Csv -Path $outputFile -NoTypeInformation
    Write-Host "Exported $tableName to $outputFile"
}