
module Predicate_mod
  use Event_mod
  use StringConversionUtilities_mod, only : MAXLEN_STRING
  use EventPolyWrapVector_mod
  implicit none
  private

  public :: Predicate, newPredicate

  type, abstract :: Predicate
     character(len=MAXLEN_STRING) :: name
   contains
     procedure(verifyInterface), deferred :: verify
  end type Predicate

  abstract interface
     logical function verifyInterface(this,subj,eventList)
       import Predicate
       import EventPolyWrapVector
       class (Predicate), intent(inout) :: this
       character(*), intent(in) :: subj
       type(EventPolyWrapVector), intent(in) :: eventList
     end function verifyInterface
  end interface

contains

  type(Predicate) function newPredicate(name) result(pred_)
    character(*) :: name
    pred_%name = name
  end function newPredicate

!  logical function verifyAgainst(this,subj_,eventList) result(ok)
!    class(Predicate), intent(inout) :: this
!    character(*) :: subj_
!    type(EventPolyWrapVector), intent(in) :: eventList
!    ok = .false.
!    call throw('Predicate%verifyAgainst::Error:NotImplemented')
!  end function verifyAgainst

end module Predicate_mod


     
