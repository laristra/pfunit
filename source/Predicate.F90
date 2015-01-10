
module Predicate_mod
  use StringConversionUtilities_mod, only : MAXLEN_STRING
  implicit none
  private

  public :: Predicate, newPredicate

  type :: Predicate
     character(len=MAXLEN_STRING) :: name
   contains
     procedure :: verifyAgainst
  end type Predicate

contains

  type(Predicate) function newPredicate(name) result(pred_)
    character(*) :: name
    pred_%name = name
  end function newPredicate

  logical function verifyAgainst(this,subj_) result(ok)
    class(Predicate), intent(inout) :: this
    character(*) :: subj_
    ok = .false.
  end function verifyAgainst

end module Predicate_mod


     
