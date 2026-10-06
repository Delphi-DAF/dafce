unit Daf.CommandLine.Resources;

{
  The texts the command line writes to whoever invokes the tool: every parse error, the words an
  error uses to describe an arity, and what reading a refused value raises.

  They are resourcestrings, in a unit of their own, so they can be replaced without touching the code
  that writes them.
}

interface

resourcestring
  SArityNone = 'no value';
  SArityAtMostOne = 'at most one value';
  SArityExactlyOne = 'exactly one value';
  SArityAny = 'any number of values';
  SArityAtLeastOne = 'at least one value';

  SUnknownOption = 'Unknown option "%s" for %s';
  SUnexpectedArgument = 'Unexpected argument "%s" for %s';
  SUnknownOptionInGroup = 'Unknown option "%s" inside the group "%s", so nothing in the group is taken';

  SResponseFileTooDeep = 'Response file "%s" is nested deeper than %d, which is what a file naming itself looks like';
  SResponseFileMissing = 'Response file "%s" does not exist';
  SResponseFileUnreadable = 'Response file "%s" could not be read: %s';

  SOptionRepeated = 'Option "%s" was given more than once, and it takes %s';
  SOptionRequiresValue = 'Option "%s" requires a %s value';
  SCommandRequiresSubcommand = 'Command "%s" requires a subcommand: %s';
  SOptionRequired = 'Option "%s" is required';
  SArgumentRequired = 'Argument "%s" is required';

  SValueNotAccepted = '"%s" is not a valid value for %s. Accepted: %s';
  SValueNotConverted = '"%s" is not a valid %s for %s';
  SValueRefused = '"%s" is not a valid value for %s';
  SPathMissing = '"%s" does not exist, and %s only accepts a path that does';
  SFileNameIllegal = '"%s" is not a legal file name, and %s accepts only one that is';

  SValueNotProvided = '%s is required but was not provided';

implementation

end.
