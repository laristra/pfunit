
module PredicateWasCalledOnce_mod
  use Event_mod
  use EventPolyWrapVector_mod
  use Predicate_mod
  implicit none
  private

  public :: WasCalledOnce

  type, extends(Predicate) :: WasCalledOnce
   contains
!     procedure :: verifyAgainst
     procedure :: verify => verify_WasCalledOnce
  end type WasCalledOnce

  !!!! Or do we invoke the predicate from the subject?

contains

!  logical function verifyAgainst(this,subj_) result(ok)
!    class(WasCalledOnce), intent(inout) :: this
!    character(*) :: subj_
!    ok = .false.
!    call throw('PredicateWasCalledOnce%verifyAgainst::Error:NotImplemented')
!  end function verifyAgainst


  logical function verify_WasCalledOnce(this,subj_,eventList) result(verifiedp_)
    class (WasCalledOnce), intent(inout) :: this
    character(*) :: subj_
    class (Event), pointer :: event_
    type (EventPolyWrapVector) :: eventList
    type (EventPolyWrapVectorIterator) :: eventListIterator
    logical :: ok
    verifiedp_ = .false.
    

! TODO: Go over EventList (registry) to determine if the Expectation was met.
! Q:  What does an expectation depend on?  Certainly the history of the run. 
!     How about other expectations?
    !

    ! Use a subject object...
    !ok = GlobalMockRepository%countTimesCalled(this%subj%name) .ge. 1
    ! Use a string...
    ok = MockRepositoryPointer%countTimesCalled(subj_) .eq. 1

    if(.not.ok)then
       call throw('             "'// &
            & trim(subj_)//'" "'//trim(this%name)//'" does not hold.')
!            & trim(exp%subj%name)//'" "'//trim(exp%pred%name)//'" does not hold.')
    else
       verifiedp_ = .false.
    end if

  end function verify
  

end module PredicateWasCalledOnce_mod

! Not English...
! call subject%verify(wasCalled)

! in verifier
! next subject, next exp -> wasCalled
! call this%verify(subject,wasCalled)
! sub verify -> wasCalled%verifyAgainst(subject)

! or in Verifier::verify
! if wasCalled%verifyAgainst(subj,exp) then...
!


