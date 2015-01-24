
module PredicateWasCalled_mod
  use Event_mod
  use EventPolyWrap_mod
  use EventPolyWrapVector_mod
  use Exception_mod
  use Predicate_mod
  implicit none
  private

  public :: WasCalled
!  public :: WasCalled, nWasCalled

  type, extends(Predicate) :: WasCalled
   contains
!     procedure :: verifyAgainst
     procedure :: verify => verify_WasCalled
  end type WasCalled

!!!! Or do we invoke the predicate from the subject?

  interface WasCalled
     module procedure newWasCalled
  end interface WasCalled

contains

  type(WasCalled) function newWasCalled() result(pred_)
    pred_%name = 'wasCalled'
  end function newWasCalled

!  logical function verifyAgainst(this,subj_) result(ok)
!    class(WasCalled), intent(inout) :: this
!    character(*) :: subj_
!    ok = .false.
!    call throw('PredicateWasCalled%verifyAgainst::Error:NotImplemented')
!  end function verifyAgainst


  logical function verify_WasCalled(this,subj,eventList) result(verifiedp_)
    class (WasCalled), intent(inout) :: this
    character(*), intent(in) :: subj
    type (EventPolyWrapVector), intent(in) :: eventList
    
    class (Event), pointer :: event_
    type (EventPolyWrap), pointer :: eventContainer
    type (EventPolyWrapVectorIterator) :: iter
    logical :: ok
    integer :: nCalls
    
    verifiedp_ = .false.

! TODO: Go over EventList (registry) to determine if the Expectation was met.
! Q:  What does an expectation depend on?  Certainly the history of the run. 
!     How about other expectations?
    !

    !print *,10000
    
    iter = eventList%begin()
    ! if no events occurred, then this is not an error.
    !if (iter == eventList%end()) then
    !   verifiedp_ = .false.
    !   call throw('PredicateWasCalled%verify::Error: Empty event list!')
    !   return
    !end if
    
    !print *,11000
    
    nCalls = 0
    do while (iter /= eventList%end())
       eventContainer => iter%get()
       event_ => eventContainer%get()
       ! print *,11500,"'"//trim(this%name)//"'","'"//trim(event_%getMessage())//"'"
       if(subj == event_%getName()) then
          if(trim(this%name) == trim(event_%getMessage()))then
             nCalls=nCalls+1
          end if
       end if
       call iter%next()
    end do

    !print *,12000
    
    ! Use a subject object...
    !ok = GlobalMockRepository%countTimesCalled(this%subj%name) .ge. 1
    ! Use a string...
    !ok = MockRepositoryPointer%countTimesCalled(subj) .ge. 1
    !
    ok = nCalls .ge. 1

    !print *,13000

    if(.not.ok)then
       !print *,13010
       call throw('             "'// &
            & trim(subj)//'" "'//trim(this%name)//'" does not hold.')
!            & trim(exp%subj%name)//'" "'//trim(exp%pred%name)//'" does not hold.')
    else
       !print *,13020
       verifiedp_ = .false.
    end if

    !print *,14000

  end function verify_WasCalled
  

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


