
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
     procedure :: verify_ => verify_WasCalled
     procedure :: verify_i1_ => verify_i1_WasCalled
     procedure :: verify_p1_ => verify_p1_WasCalled
     !generic :: verify => verify_, verify_i1_
     procedure :: argumentsToBeVerified => argumentsToBeVerified_
  end type WasCalled

!!!! Or do we invoke the predicate from the subject?

  interface WasCalled
     module procedure newWasCalled
  end interface WasCalled

  interface verify
     module procedure verify_WasCalled
     !module procedure verify_i1_WasCalled
     module procedure verify_p1_WasCalled
  end interface verify

contains

  type(WasCalled) function newWasCalled() result(pred_)
    pred_%name = 'wasCalled'
  end function newWasCalled

  logical function argumentsToBeVerified_(this)
    class(WasCalled), intent(inout) :: this
    !print *,20000,this%name
    argumentsToBeVerified_ = .false.
  end function argumentsToBeVerified_

!  logical function verifyAgainst(this,subj_) result(ok)
!    class(WasCalled), intent(inout) :: this
!    character(*) :: subj_
!    ok = .false.
!    call throw('PredicateWasCalled%verifyAgainst::Error:NotImplemented')
!  end function verifyAgainst

  ! Note:  Maybe wasCalled should be hasBeenCalled to get the implications for
  !        timing right.  Also as currently implemented, this immediately throws an
  !        exeception if it fails, without regard to whether testing has been
  !        completed or not.  If verify is executed by a mock during the test
  !        and we are iterating over the expectation list, some of those predicates
  !        should not be active until the test is over.
  
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

    ! Should this predicate be "WasCalledAtEnd?" as implied by the following line?
    if (.not.finalVerificationEnabled(EventList)) return

! TODO: Go over EventList (registry) to determine if the Expectation was met.
! Q:  What does an expectation depend on?  Certainly the history of the run. 
!     How about other expectations?

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
       !print *,11500,"'"//trim(this%name)//"'","'"//trim(event_%getMessage())//"'"
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


  logical function verify_i1_WasCalled(this,subj,eventList&
       & ,i1 )
    class (WasCalled), intent(inout) :: this
    character(*), intent(in) :: subj
    type(EventPolyWrapVector), intent(in) :: eventList
    integer, intent(in) :: i1
    call throw('verify_i1_::not implemented')
    verify_i1_WasCalled = .false.
  end function verify_i1_WasCalled

  logical function verify_p1_WasCalled(this,subj,eventList&
       &, p1 )
    class (WasCalled), intent(inout) :: this
    character(*), intent(in) :: subj
    type(EventPolyWrapVector), intent(in) :: eventList
    class(*), intent(in) :: p1
    call throw('verify_p1_::not implemented')
    verify_p1_WasCalled = .false.
  end function verify_p1_WasCalled

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


