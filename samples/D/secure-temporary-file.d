module tempfile ;
import tango.io.TempFile, tango.io.Stdout ;

void main(char[][] args) {

  // create a temporary file that will be deleted automatically when out of scope
  auto tempTransient = new TempFile(TempFile.Transient) ;
  Stdout(tempTransient.path()).newline ;

  // create a temporary file, still persist after the TempFile object has been destroyed
  auto tempPermanent = new TempFile(TempFile.Permanent) ;
  Stdout(tempPermanent.path()).newline ;

  // both can only be accessed by the current user (the program?).
}
