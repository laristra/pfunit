!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: MockRepository
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune, NASA/GSFC 
!!
!! @date
!! 07 Nov 2013
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 07 Nov 2013 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------
module MockRepository_mod
   use Expectation_mod, only : Expectation, newExpectation, ExpectationThat
   use Expectation_mod, only : Subject, newSubject
   use Expectation_mod, only : Predicate

   use Event_mod
   use EventPolyWrap_mod
   use EventPolyWrapVector_mod
   use ExpectationPolyWrap_mod
   use ExpectationPolyWrapVector_mod

   use Predicates_mod

   ! uze Verifier_mod
   
   implicit none
   private

   public :: MockRepository
   public :: newMockRepository
   public :: MockRepositoryPointer

   public :: MAX_LEN_METHOD_NAME
   public :: MAX_LEN_CALL_REGISTRATION

   integer, parameter :: MAX_LEN_METHOD_NAME = 32
   integer, parameter :: MAX_LEN_CALL_REGISTRATION = 32
   integer, parameter :: MAX_EXPECTATIONS = 8

 
   type MockRepository
      ! mlr todo Allocatable strings are available...
      ! mlr todo test under nag
      character(len=MAX_LEN_METHOD_NAME) :: method = ' '
      type(Expectation) :: Expectations(MAX_EXPECTATIONS)
      integer :: lastExpectation = 0
      !   character(len=MAX_LEN_CALL_REGISTRATION) :: callRegistry(2) ! make dynamic...

      ! callRegistry needs to be replaced by an EventList
      ! rather:  EventPolyWrapVector...
      !
      type(Expectation) :: callRegistry(2)
      integer :: lastRegistration = 0

      type(EventPolyWrapVector)       :: EventList
      type(ExpectationPolyWrapVector) :: ExpectationList

    contains

      generic   :: add => addExpectation_
      procedure :: addExpectation_
      
      procedure :: registerMockCallBy_subName_
      procedure :: registerMockCallBy_subName_p1_
      !procedure :: registerMockCallBy_subName_p2_
      generic   :: registerMockCallBy => &
           & registerMockCallBy_subName_, &
           & registerMockCallBy_subName_p1_

      procedure :: enableFinalVerification

      procedure :: verify
      procedure :: verifyArguments_p1_
      procedure :: verifyArguments => verifyArguments_p1_
      ! generic :: <verifyGeneric> => verify, verifyArguments

      ! final?
      ! mlr todo make a deep delete

      ! Since Tests in TestCases and Suites are in some sense
      ! recursive, we need to have a stack of MockRepositories.
      ! Now some of these MRs will not actually do anything,
      ! but we still need them because it's hard to know when
      ! we have tests or not when we're bouncing up and down
      ! Test and Suite levels.
      !
      ! Delete pops the latest MR off of the stack. As we unwind
      ! off the test hierarchy, we'll delete all of the MRs.
      !
      procedure :: delete

   end type MockRepository

   interface add_
      module procedure addExpectation_
   end interface add_
   
   interface registerMockCallBy_
      module procedure registerMockCallBy_subName_
   end interface registerMockCallBy_

   !interface verifyArguments
   !   ! The explicit interface may not be needed.
   !   subroutine verifyArguments_i1_(subj,i1)
   !     character(len=*), intent(in) :: subj
   !     integer, intent(in) :: i1
   !   end subroutine verifyArguments_i1_
   !end interface verifyArguments

   !interface verifyArguments
   !   module procedure verifyArguments_i1_
   !end interface verifyArguments

   type :: MockRepositoryContainer
      class (MockRepository), pointer :: ptr
   end type MockRepositoryContainer

   ! Global singleton mock repository pointer.
   !
   class (MockRepository), pointer :: MockRepositoryPointer => null()
   !
   ! Kluge because we recurse through testCases/Suites.
   ! Need to understand ramifications for MPI use.
   !
   integer, parameter :: MockRepositoryStack_MAX = 8
   integer            :: MockRepositoryStack_Top = 0
   type (MockRepositoryContainer), dimension(MockRepositoryStack_MAX) :: &
        & MockRepositoryStack

contains

  ! Allocate a new MockRepository on the top of the repository stack.
  function newMockRepository() result(repository)
    use Exception_mod
      type (MockRepository), pointer :: repository

      MockRepositoryStack_Top = MockRepositoryStack_Top + 1
      if (MockRepositoryStack_Top > MockRepositoryStack_MAX) then
         call throw('MockRepository%newMockRepository::ERROR::StackException-TooManyTestLevels')
      end if
      
      allocate(repository); MockRepositoryPointer => repository
      repository%lastExpectation  = 0
      repository%lastRegistration = 0

      ! push the MR onto the stack.
      MockRepositoryStack(MockRepositoryStack_Top)%ptr => repository
      
   end function newMockRepository

   ! Delete this MockRepository, moving the stack pointer to
   ! the previous repository.
   subroutine delete (this)
     use Exception_mod
     class (MockRepository), intent(inout) :: this
     
     if (.not.associated(MockRepositoryPointer)) then
        call throw('MockRepository%delete::ERROR::RepositoryNotAllocated')
     else
        nullify(MockRepositoryPointer)

        ! Assume stack is okay coming in.
        nullify(MockRepositoryStack(MockRepositoryStack_Top)%ptr)

        ! pop the MR off the stack (decrement top)
        MockRepositoryStack_Top = MockRepositoryStack_Top - 1
        ! Careful with the logic... 0 is an empty stack... Just leave things alone...
        if (MockRepositoryStack_Top < 0) then
           call throw('MockRepository%newMockRepository::ERROR::StackException-Underflow')
        else if (MockRepositoryStack_Top > 0) then
           MockRepositoryPointer => MockRepositoryStack(MockRepositoryStack_Top)%ptr
        end if
        
     end if

   end subroutine delete

   subroutine addExpectation_(this,expectation_)
     use Exception_mod
     class (MockRepository), intent(inout) :: this
     type(Expectation), intent(in) :: expectation_

     call this%ExpectationList%push_back(ExpectationPolyWrap(expectation_))

   end subroutine addExpectation_

   subroutine registerMockCallBy_subName_(this,subName)
     class (MockRepository), intent(inout) :: this
     character(len=*), intent(in) :: subName
     call this%EventList%push_back(EventPolyWrap(newEvent(subName,"wasCalled")))
   end subroutine registerMockCallBy_subName_

   subroutine registerMockCallBy_subName_p1_(this,subName,p1)
     class (MockRepository), intent(inout) :: this
     character(len=*), intent(in) :: subName
     class(*), intent(in) :: p1     
     call this%EventList%push_back(EventPolyWrap(newEvent(subName,"wasCalled")))
     call this%verifyArguments_p1_(subName,p1)
   end subroutine registerMockCallBy_subName_p1_

!   subroutine registerMockCallBy_subName_p2_(this,subName,p1,p2)
!     class (MockRepository), intent(inout) :: this
!     character(len=*), intent(in) :: subName
!     class(*), intent(in) :: p1,p2
!     call this%EventList%push_back(EventPolyWrap(newEvent(subName,"wasCalled")))
!     call this%verifyArguments_p2_(subj,p1,p2)
!   end subroutine registerMockCallBy_subName_p2_

! TODO: Is this the correct place to keep/use/verify expectations?  Or should we be more like
! exceptions?
!
   subroutine verify(this)
      use Exception_mod
      class (MockRepository), intent(inout), target :: this
      class (Event), pointer :: lastEvent
      class (Expectation), pointer :: exp, reg
      class (ExpectationPolyWrap), pointer :: expContainer
      type (ExpectationPolyWrapVectorIterator) :: iter
      logical ok

      iter = this%expectationList%begin()

      ! Traverse store of expectations and call on their verification methods.
      ok = .true.
      do while (iter /= this%expectationList%end())
         expContainer => iter%get(); exp => expContainer%get()

         !print *,'2000: ',trim(exp%subj%name)//'...'//trim(exp%pred%name)
         ok = ok .and. exp%verify(this%EventList)
         !??if (.not.ok) then
         !??   ! .not.ok => an exception was thrown by verify.
         !??   ! Should we retun immediately, or continue through the exceptions?
         !??   return
         !??end if
         call iter%next()
      end do

      return

    end subroutine verify

    subroutine enableFinalVerification(this)
      use Exception_mod
      use Predicates_mod, only: finalVerificationEnabledMessage
      class (MockRepository), intent(inout), target :: this

      ! This event should be the last one on the EventList, but we're neither
      ! enforcing or testing this at this point.

      call this%EventList&
           &%push_back(EventPolyWrap(&
           &           newEvent('MockRepository', &
           &                    finalVerificationEnabledMessage)))
      
    end subroutine enableFinalVerification

    ! Is there a better way to handle the args here?
    !
    subroutine verifyArguments_p1_(this,subj,p1)
      use Exception_mod
      class (MockRepository), intent(inout), target :: this
      character(len=*), intent(in) :: subj
      class(*), intent(in) :: p1
      
      class (Event), pointer :: lastEvent
      class (Expectation), pointer :: exp, reg
      class (ExpectationPolyWrap), pointer :: expContainer
      type (ExpectationPolyWrapVectorIterator) :: iter
      logical ok
      iter = this%expectationList%begin()
      ! Traverse store of expectations and call on their verification methods.
      ! Only execute those that have arguments.

      !call throw('MockRepository%verifyArguments_i1_::not implemented')
      
      ! Iterate over expectations. Ask active ones to verify.
      ! Wouldn't it be nicer to push the arguments onto an object and use
      ! polymorphism to get rid of the duplicity of definitions of this
      ! subroutine?

      ok = .true.
      do while (iter /= this%expectationList%end())
         expContainer => iter%get(); exp => expContainer%get()
         !print *,'1000: ',trim(exp%subj%name)//'...'//trim(exp%pred%name)
         if (exp%argumentsToBeVerified()) then
            ! Can we repace i1 in the following with a polymorphic quantity? YES!
            ok = ok .and. exp%verify(this%EventList,p1)
            !print *,'1100: ',trim(exp%subj%name)//'...'//trim(exp%pred%name)
            !?if (.not.ok) then
            !?   ! .not.ok => an exception was thrown by verify.
            !?   ! Should we retun immediately, or continue through the exceptions?
            !?   return
            !?end if
         end if
         call iter%next()
      end do
      return
    end subroutine verifyArguments_p1_

!    subroutine verifyArguments_p2_(this,subj,p1,p2)
!      use Exception_mod
!      class (MockRepository), intent(inout), target :: this
!      character(len=*), intent(in) :: subj
!      class(*), intent(in) :: p1,p2
!      
!      class (Event), pointer :: lastEvent
!      class (Expectation), pointer :: exp, reg
!      class (ExpectationPolyWrap), pointer :: expContainer
!      type (ExpectationPolyWrapVectorIterator) :: iter
!      logical ok
!      iter = this%expectationList%begin()
!      ! Traverse store of expectations and call on their verification methods.
!      ! Only execute those that have arguments.
!
!      !call throw('MockRepository%verifyArguments_i1_::not implemented')
!      
!      ! Iterate over expectations. Ask active ones to verify.
!      ! Wouldn't it be nicer to push the arguments onto an object and use
!      ! polymorphism to get rid of the duplicity of definitions of this
!      ! subroutine?
!
!      ok = .true.
!      do while (iter /= this%expectationList%end())
!         expContainer => iter%get(); exp => expContainer%get()
!         !print *,'1000: ',trim(exp%subj%name)//'...'//trim(exp%pred%name)
!         if (exp%argumentsToBeVerified()) then
!            ! Can we repace i1 in the following with a polymorphic quantity? YES!
!            ok = ok .and. exp%verify(this%EventList,p1,p2)
!            !print *,'1100: ',trim(exp%subj%name)//'...'//trim(exp%pred%name)
!            !?if (.not.ok) then
!            !?   ! .not.ok => an exception was thrown by verify.
!            !?   ! Should we retun immediately, or continue through the exceptions?
!            !?   return
!            !?end if
!         end if
!         call iter%next()
!      end do
!      return
!    end subroutine verifyArguments_p2_
    
end module MockRepository_mod
