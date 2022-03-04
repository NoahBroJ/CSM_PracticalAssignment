fslex "%~dp0\PracticalAssignment\Lexer.fsl" --unicode
fsyacc "%~dp0\PracticalAssignment\Parser.fsp" --module Parser
dotnet fsi "%~dp0\PracticalAssignment\Main.fsx"
PAUSE