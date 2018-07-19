use Term::ReadKey;
ReadMode 'restore';    # Flush the keyboard and returns input stream to initial state
# ReadMode 0;            # Numerical equivalent of keyboard restore (move comment marker to use instead)

# A more complete example for use in keyboard handler programming.
# We should also check we are being used in an interactive context (not done here).

use Term::ReadKey;
ReadMode 'cbreak';

# Flush the keyboard in terminal character break mode
while (defined ReadKey -1) {
  # Do nothing
}

# Don't forget to restore the readmode, when we are finished using the keyboard
ReadMode 'restore';
