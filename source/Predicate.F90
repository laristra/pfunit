
module Predicate_mod
  use Event_mod
  use EventPolyWrap_mod
  use EventPolyWrapVector_mod
  use StringConversionUtilities_mod, only : MAXLEN_STRING
  implicit none
  private

  public :: Predicate
  public :: finalVerificationEnabled
  public :: finalVerificationEnabledMessage

  ! Do we need a messages module?  Should this be promoted to be a predicate?
  ! Maybe the following can be made into a predicate when we have compound
  ! predicates.
  !
  ! Some predicates are only active after the test execution is complete.
  !
  character(len=*), parameter :: &
       & finalVerificationEnabledMessage = 'finalVerificationEnabled'

  type, abstract :: Predicate
     character(len=MAXLEN_STRING) :: name
   contains
     procedure(verifyInterface), deferred :: verify_
     procedure(verify_i1_Interface), deferred :: verify_i1_
     ! generic :: verify => verify_
     generic :: verify => verify_, verify_i1_
     procedure(argumentsToBeVerifiedInterface), deferred :: &
          & argumentsToBeVerified
  end type Predicate

  abstract interface
     
     logical function verifyInterface(this,subj,eventList)
       import Predicate
       import EventPolyWrapVector
       class (Predicate), intent(inout) :: this
       character(*), intent(in) :: subj
       type(EventPolyWrapVector), intent(in) :: eventList
     end function verifyInterface

     logical function verify_i1_Interface(this,subj,eventList&
          & ,i1 )
       import Predicate
       import EventPolyWrapVector
       class (Predicate), intent(inout) :: this
       character(*), intent(in) :: subj
       type(EventPolyWrapVector), intent(in) :: eventList
       integer, intent(in) :: i1
     end function verify_i1_Interface

     logical function argumentsToBeVerifiedInterface(this)
       import Predicate
       class (Predicate), intent(inout) :: this
     end function argumentsToBeVerifiedInterface
     
  end interface

contains

!?  type(Predicate) function newPredicate(name) result(pred_)
!?    character(*) :: name
!?    pred_%name = name
!?  end function newPredicate

!  logical function verifyAgainst(this,subj_,eventList) result(ok)
!    class(Predicate), intent(inout) :: this
!    character(*) :: subj_
!    type(EventPolyWrapVector), intent(in) :: eventList
!    ok = .false.
!    call throw('Predicate%verifyAgainst::Error:NotImplemented')
!  end function verifyAgainst

  logical function finalVerificationEnabled(EventList) result(enabled)
    class(EventPolyWrapVector), intent(in) :: EventList
    class(EventPolyWrap), pointer :: eventContainer
    class(Event), pointer :: event_

    if (EventList%empty()) then
       enabled = .false.
       return
    end if
    
    eventContainer => EventList%back()
    event_ => eventContainer%get()
    
    if (event_%getMessage() &
         & == finalVerificationEnabledMessage) then
       enabled = .true.
    else
       enabled = .false.
    end if
    
!    enabled = &
!         & EventList%back()%get()%getMessage() == finalVerificationEnabledMessage
  end function finalVerificationEnabled
  
end module Predicate_mod


     
