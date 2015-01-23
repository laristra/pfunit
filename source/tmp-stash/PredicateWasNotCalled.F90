
module PredicateWasNotCalled_mod
  use Event_mod
  use EventPolyWrapVector_mod
  use Predicate_mod
  implicit none
  private

  public WasNotCalled

  type, extends(Predicate) :: WasNotCalled
   contains
     procedure :: verify
  end type WasNotCalled

contains
    
  logical function verify(this,subj_,eventList) result(verifiedp_)
    class (WasNotCalled), intent(inout) :: this
    character(*) :: subj_
    class (Event), pointer :: event_
    type (EventPolyWrapVector) :: eventList
    type (EventPolyWrapVectorIterator) :: eventListIterator
    logical :: ok
    verifiedp_ = .false.

    ok = MockRepositoryPointer%countTimesCalled(subj_) .eq. 0

    if(.not.ok)then
       call throw('             "'// &
            & trim(subj_)//'" "'//trim(this%name)//'" does not hold.')
!            & trim(exp%subj%name)//'" "'//trim(exp%pred%name)//'" does not hold.')
    else
       verifiedp_ = .false.
    end if

  end function verify
  
end module PredicateWasNotCalled_mod
