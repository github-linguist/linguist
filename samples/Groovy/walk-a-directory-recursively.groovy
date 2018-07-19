new File('.').eachFileRecurse {
  if (it.name =~ /.*\.txt/) println it;
}
