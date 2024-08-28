/**
 * SemVer.org v2
 * https://semver.org/spec/v2.0.0.html
 * For unit tests see: https://github.com/dselman/peggy-semver
 */

semver
  = versionCore:versionCore pre:('-' @preRelease)? build:('+' @build)? {
    return { ...versionCore, pre, build };
  }

versionCore
  = major:numericIdentifier '.' minor:numericIdentifier '.' patch:numericIdentifier {
    return { major, minor, patch };
  }

preRelease
  = head:preReleaseIdentifier tail:('.' @preReleaseIdentifier)* {
    return [ head, ...tail ];
  }

build
  = head:buildIdentifier tail:('.' @buildIdentifier)* {
    return [ head, ...tail ];
  }

preReleaseIdentifier
  = alphanumericIdentifier
  / numericIdentifier

buildIdentifier
  = alphanumericIdentifier
  / $digit+ // Not a number, buildIdentifiers aren't semantically significant.

// If there is a non-digit anywhere, this label is alphanumeric, and
// is compared lexically.  Return a string.
alphanumericIdentifier
  = $(digit* nonDigit identifierChar*)

// Any semantically significant numbers are turned into BigInts (note: there
// is no length maximum for numbers in semver) for later numeric comparison.
numericIdentifier
  = n:('0' / $(positiveDigit digit*)) { return BigInt(n); }

identifierChar
  = [a-z0-9-]i

nonDigit
  = [a-z-]i

digit
  = [0-9]

positiveDigit
  = [1-9]
