(** Executor is responsible for the execution of [Command.command] objects *)

exception BadArgument
(** [BadArgument] is thrown when an argument causes an error in execution *)

exception UnexpectedError
(** [UnexpectedError] is thrown when an unforseeable and possibly
    un-duplicatable error occurs, such as a connection error. *)

val execute : Command.command -> string list
(** [execute] is responsible for calling the correct functions necessary to
    execute a user's command. Raises [BadArgument] if the provided arguments
    don't work in context, and raises [UnexpectedError] in the case of
    unpredictable and unpreventable errors like network issues.

    For example, if the command input represents the user command "committees
    John Fetterman", this function should return a list of Senator Fetterman's
    committees.

    Many commands warrant a single line input intead of multiple (unlike the
    senator example). In this case, the returned list will likely only contain
    one string. *)
