namespace Shared

type GitCommand = | Status

type ProcessRequestType = Git of GitCommand

type ProcessRequest = {
    Type: ProcessRequestType
    FileName: string
    Args: string
    WorkingDir: string
}

type ProcessResponse =
    | Output of string
    | Failure of string
