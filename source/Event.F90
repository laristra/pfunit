
module Event_mod
  use StringConversionUtilities_mod, only : MAXLEN_STRING
  implicit none
  private

  public :: Event, newEvent

  type :: Event
     character(len=MAXLEN_STRING) :: name
  end type Event

contains

  type(Event) function newEvent(name) result(event_)
    character(*) :: name
    event_%name = name
  end function newEvent

end module Event_mod
  
