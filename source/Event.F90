!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: Event
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Mike Rilee, Rilee Systems Technologies
!!
!! @date
!! 29 Jan 2015
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 29 Jan 2015 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------



module Event_mod
  use StringConversionUtilities_mod, only : MAXLEN_STRING
  implicit none
  private

  public :: Event, newEvent

  type :: Event
     character(len=:), allocatable :: name
     character(len=:), allocatable :: message
   contains
     procedure :: getName
     procedure :: getMessage
  end type Event

contains

  type(Event) function newEvent(name,message) result(event_)
    character(len=*) :: name, message
    event_%name = name; event_%message = message
  end function newEvent

  function getName(this) result(name_)
    class (Event), intent(in) :: this
    character(len=:), allocatable :: name_
    name_ = this%name
  end function getName

  function getMessage(this) result(msg_)
    class (Event), intent(in) :: this
    character(len=:), allocatable :: msg_
    msg_ = this%message
  end function getMessage

end module Event_mod
  
