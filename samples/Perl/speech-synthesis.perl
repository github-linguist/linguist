use Speech::Synthesis;

($engine) = Speech::Synthesis->InstalledEngines();
($voice) = Speech::Synthesis->InstalledVoices(engine => $engine);

Speech::Synthesis
  ->new(engine => $engine, voice => $voice->{id})
  ->speak("This is an example of speech synthesis.");
