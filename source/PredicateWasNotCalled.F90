!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: PredicateWasNotCalled
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Mike Rilee, Rilee Systems Technology
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

module PredicateWasNotCalled_mod
  use Event_mod
  use EventPolyWrap_mod
  use EventPolyWrapVector_mod
  use Exception_mod
  use Predicate_mod
  implicit none
  private

  public :: WasNotCalled
!  public :: WasNotCalled, nWasNotCalled

  type, extends(Predicate) :: WasNotCalled
   contains
!     procedure :: verifyAgainst
     procedure :: verify_ => verify_WasNotCalled
     procedure :: verify_i1_ => verify_i1_WasNotCalled
     procedure :: verify_p1_ => verify_p1_WasNotCalled
     procedure :: argumentsToBeVerified => argumentsToBeVerified_     
  end type WasNotCalled

!!!! Or do we invoke the predicate from the subject?

  interface WasNotCalled
     module procedure newWasNotCalled
  end interface WasNotCalled

  interface verify
     module procedure verify_WasNotCalled
     !module procedure verify_i1_WasNotCalled
     module procedure verify_p1_WasNotCalled
  end interface verify

contains

  type(WasNotCalled) function newWasNotCalled() result(pred_)
    pred_%name = 'wasNotCalled'
  end function newWasNotCalled

  logical function argumentsToBeVerified_(this)
    class(WasNotCalled), intent(inout) :: this
    argumentsToBeVerified_ = .false.
  end function argumentsToBeVerified_  

!  logical function verifyAgainst(this,subj_) result(ok)
!    class(WasNotCalled), intent(inout) :: this
!    character(*) :: subj_
!    ok = .false.
!    call throw('PredicateWasNotCalled%verifyAgainst::Error:NotImplemented')
!  end function verifyAgainst

  logical function verify_WasNotCalled(this,subj,eventList) result(verifiedp_)
    class (WasNotCalled), intent(inout) :: this
    character(*), intent(in) :: subj
    type (EventPolyWrapVector), intent(in) :: eventList
    
    class (Event), pointer :: event_
    type (EventPolyWrap), pointer :: eventContainer
    type (EventPolyWrapVectorIterator) :: iter
    logical :: ok
    integer :: nCalls
    
    verifiedp_ = .false.

    !call throw('PredicateWasNotCalled::NotImplemented')
    !return

! TODO: Go over EventList (registry) to determine if the Expectation was met.
! Q:  What does an expectation depend on?  Certainly the history of the run. 
!     How about other expectations?
    !

    !print *,10000
    
    iter = eventList%begin()
    ! if no events occurred, then this is not an error.
    !if (iter == eventList%end()) then
    !   verifiedp_ = .false.
    !   call throw('PredicateWasNotCalled%verify::Error: Empty event list!')
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
    ok = nCalls .eq. 0

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

  end function verify_WasNotCalled

  logical function verify_i1_WasNotCalled(this,subj,eventList&
       & ,i1 )
    class (WasNotCalled), intent(inout) :: this
    character(*), intent(in) :: subj
    type(EventPolyWrapVector), intent(in) :: eventList
    integer, intent(in) :: i1
    call throw('verify_i1_::not implemented')
    verify_i1_WasNotCalled = .false.
  end function verify_i1_WasNotCalled

  logical function verify_p1_WasNotCalled(this,subj,eventList&
       &, p1 )
    class (WasNotCalled), intent(inout) :: this
    character(*), intent(in) :: subj
    type(EventPolyWrapVector), intent(in) :: eventList
    class(*), intent(in) :: p1
    call throw('verify_p1_::not implemented')
    verify_p1_WasNotCalled = .false.
  end function verify_p1_WasNotCalled

end module PredicateWasNotCalled_mod

