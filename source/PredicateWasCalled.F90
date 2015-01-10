
module PredicateWasCalled_mod

  use MockRepository_mod, only: MockRepositoryPointer
  use Predicate_mod
  implicit none
  private

  public :: WasCalled

  type, extends(Predicate) :: WasCalled
   contains
     procedure :: verifyAgainst
     procedure :: verify
  end type WasCalled

  !!!! Or do we invoke the predicate from the subject?

contains

  logical function verifyAgainst(this,subj_) result(ok)
    class(WasCalled), intent(inout) :: this
    character(*) :: subj_
    ok = .false.
  end function verifyAgainst


  logical function verify(this,subj_) result(verifiedp_)
    class (WasCalled), intent(in) :: this
    character(*) :: subj_
    logical :: ok
    verifiedp_ = .false.

! TODO: Go over EventList (registry) to determine if the Expectation was met.
! Q:  What does an expectation depend on?  Certainly the history of the run. 
!     How about other expectations?
    !

    ! Use a subject object...
    !ok = GlobalMockRepository%countTimesCalled(this%subj%name) .ge. 1
    ! Use a string...
    ok = MockRepositoryPointer%countTimesCalled(subj_) .ge. 1

    if(.not.ok)then
       call throw('             "'// &
            & trim(subj_)//'" "'//trim(this%name)//'" does not hold.')
!            & trim(exp%subj%name)//'" "'//trim(exp%pred%name)//'" does not hold.')
    else
       verifiedp_ = .false.
    end if

  end function verify
  

end module PredicateWasCalled_mod

! Not English...
! call subject%verify(wasCalled)

! in verifier
! next subject, next exp -> wasCalled
! call this%verify(subject,wasCalled)
! sub verify -> wasCalled%verifyAgainst(subject)

! or in Verifier::verify
! if wasCalled%verifyAgainst(subj,exp) then...
!


