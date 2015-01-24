
module PredicateWasCalledOnce_mod
  use Event_mod
  use EventPolyWrap_mod
  use EventPolyWrapVector_mod
  use Exception_mod
  use Predicate_mod
  implicit none
  private

  public :: WasCalledOnce
!  public :: WasCalledOnce, nWasCalledOnce

  type, extends(Predicate) :: WasCalledOnce
   contains
!     procedure :: verifyAgainst
     procedure :: verify => verify_WasCalledOnce
  end type WasCalledOnce

!!!! Or do we invoke the predicate from the subject?

  interface WasCalledOnce
     module procedure newWasCalledOnce
  end interface WasCalledOnce

contains

  type(WasCalledOnce) function newWasCalledOnce() result(pred_)
    pred_%name = 'wasCalledOnce'
  end function newWasCalledOnce

!  logical function verifyAgainst(this,subj_) result(ok)
!    class(WasCalledOnce), intent(inout) :: this
!    character(*) :: subj_
!    ok = .false.
!    call throw('PredicateWasCalledOnce%verifyAgainst::Error:NotImplemented')
!  end function verifyAgainst


  logical function verify_WasCalledOnce(this,subj,eventList) result(verifiedp_)
    class (WasCalledOnce), intent(inout) :: this
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
    !   call throw('PredicateWasCalledOnce%verify::Error: Empty event list!')
    !   return
    !end if
    
    !print *,11000
    
    nCalls = 0
    do while (iter /= eventList%end())
       eventContainer => iter%get()
       event_ => eventContainer%get()
       !print *,11500,"'"//trim(this%name)//"'","'"//trim(event_%getMessage())//"'"
       if(subj == event_%getName()) then
          if('wasCalled' == trim(event_%getMessage()))then
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
    ok = nCalls .eq. 1

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

  end function verify_WasCalledOnce
  

end module PredicateWasCalledOnce_mod

