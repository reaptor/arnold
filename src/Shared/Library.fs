namespace Shared

type ClientMessage =
    | GitStatus
    | GetFileContent of fileName: string

type ServerMessage =
    | GitStatusResponse of Result<string, string>
    | GetFileContentResponse of Result<string, string>
    | FileChanged
