
/*------------------------------------------------------------------------
    File        : SendEmailAlgorithm
    Purpose     :
    Syntax      :
    Description : Uses object-oriented Strategy Pattern to abstract away the
                  algorithm for sending an email by encapsulating it
                  into a data structure.
    Author(s)   : Abe Voelker
    Created     : Sat Jul 17 17:11:18 CDT 2010
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE email.SendEmailAlgorithm:

    /* Returns: */
    /* SUCCESS = empty return string */
    /* FAILURE = error message in return string */
    METHOD PUBLIC CHARACTER sendEmail(INPUT ipobjEmail AS email.Email).

END INTERFACE.
