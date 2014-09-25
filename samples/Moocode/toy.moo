@verb toy:do_the_work this none this
@program toy:do_the_work
if (this.wound)
  if ($object_utils:isa(this.location,$room))
    this.location:announce_all(this.name," ", this:continue_msg());
    this.wound = this.wound - 1;
    if (this.wound)
      fork (15)
        this:do_the_work();
      endfork
    else
      this.location:announce_all(this.name, " ", this:wind_down_msg());
    endif
  endif
  if (this.wound < 0)
    this.wound = 0;
  endif
endif
.
