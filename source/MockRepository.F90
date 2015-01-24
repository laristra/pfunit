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
!   use Expectation_mod, only : Subject, newSubject, newSubjectNameOnly
   use Expectation_mod, only : Subject, newSubject
   use Expectation_mod, only : Predicate
   ! , newPredicate
   !   use Expectation_mod, only : wasCalled, wasNotCalled, wasCalledOnce

   use Event_mod
   use EventPolyWrap_mod
   use EventPolyWrapVector_mod
   use ExpectationPolyWrap_mod
   use ExpectationPolyWrapVector_mod

   use Predicates_mod
   
   implicit none
   private

   public :: MockRepository
   public :: newMockRepository
   public :: MockRepositoryPointer

!   public :: Predicate, wasCalled
!   public :: Predicate, wasCalled, wasNotCalled, wasCalledOnce

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

      procedure :: countTimesCalled
      
      procedure :: verifyMocking
      !procedure :: expectCall
      !procedure :: hasCalled

      procedure :: expectCall
      procedure :: hasCalled

      generic   :: add => addExpectation_
      procedure :: addExpectation_
      
      generic   :: registerMockCallBy => registerMockCallBy_subName_

      procedure :: registerMockCallBy_subName_

      procedure :: verify

!      procedure :: verifyArguments

      ! final?
      ! mlr todo make a deep delete
      procedure :: delete

   end type MockRepository

   interface add_
      module procedure addExpectation_
   end interface add_
   
   interface registerMockCallBy_
      module procedure registerMockCallBy_subName_
   end interface registerMockCallBy_

   interface
      subroutine subVoid
      end subroutine subVoid
   end interface

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

!! Begin older code

  function newMockRepository() result(repository)
    use Exception_mod
      type (MockRepository), pointer :: repository
!      type (MockRepository), allocatable, target :: repository

!!??      if ( associated(MockRepositoryPointer) ) then
!!??         nullify(MockRepositoryPointer)
!!??! Sometimes TestResult%endRun isn't called!  Feature or bug?  2014-0922-2209-31-UTC MLR
!!??!         print *,'MockRepository.newMockRepository::ERROR::RepositoryAlreadyAllocated'
!!??      end if

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

   subroutine verifyMocking(this, object)
      use Exception_mod
      class (MockRepository), intent(inout) :: this
      class (*) :: object
      integer :: iExp
      class (Expectation), allocatable :: exp_
      logical :: ok
      
      if (trim(this%method) /= '') then
         call throw('Expected method not called: method1() on object of class MockSUT.')
      end if

! Begin older code
!-
!-   subroutine verifyMocking(this, object)
!-      use Exception_mod
!-      class (MockRepository), intent(inout) :: this
!-      class (*) :: object
!-      
!-      if (trim(this%method) /= '') then
!-         call throw('Expected method not called: method1() on object of class MockSUT.')
!-      end if
!-
!-!      ! Only need to verify once. Finish it off...
!-!      call this%delete()
!-
!-   end subroutine verifyMocking
!-
!-   subroutine expectCall(this, obj, method)
!-      class (MockRepository), intent(inout) :: this
!-      class(*), intent(in) :: obj
!-      character(len=*), intent(in) :: method
!-
!-      this%method = method
!-   end subroutine expectCall
!-
!-   subroutine hasCalled(this, obj, method)
!-      class (MockRepository), intent(inout) :: this
!-      class(*), intent(in) :: obj
!-      character(len=*), intent(in) :: method
!-
!-      if (trim(method) == trim(this%method)) then
!-         this%method=''
!-      end if
!-   end subroutine hasCalled
!-
!-!! End older code

   end subroutine verifyMocking

   subroutine expectCall(this, obj, method)
      class (MockRepository), intent(inout) :: this
      class(*), intent(in) :: obj
      character(len=*), intent(in) :: method

      this%method = method
   end subroutine expectCall

   subroutine hasCalled(this, obj, method)
      class (MockRepository), intent(inout) :: this
      class(*), intent(in) :: obj
      character(len=*), intent(in) :: method

      if (trim(method) == trim(this%method)) then
         this%method=''
      end if
   end subroutine hasCalled

!!!   !! End older code

   subroutine addExpectation_(this,expectation_)
     use Exception_mod
     class (MockRepository), intent(inout) :: this
     type(Expectation), intent(in) :: expectation_

     call this%ExpectationList%push_back(ExpectationPolyWrap(expectation_))

!     ! Fake implementation
!     this%lastExpectation = this%lastExpectation + 1
!     if (this%lastExpectation > MAX_EXPECTATIONS) then
!        call throw("MockRepository%addExpectation_:error:too many expectations:dropping last")
!        this%lastExpectation = this%lastExpectation - 1
!        return
!     else
!        this%Expectations(this%lastExpectation) = expectation_
!     end if
   end subroutine addExpectation_

   subroutine registerMockCallBy_subName_(this,subName)
     class (MockRepository), intent(inout) :: this
     character(len=*), intent(in) :: subName

     !     call this%EventList%push_back(EventPolywrap(newEvent(subName,"wasCalled-XXX")))
     call this%EventList%push_back(EventPolywrap(newEvent(subName,"wasCalled")))
     
!     print *,'reg200: ',subName,this%lastRegistration
     ! Can we includ the calling sub here? For a better comparison with our Exp. list?
     ! <if space in registry>
!     this%lastRegistration = this%lastRegistration + 1
!     this%callRegistry(this%lastRegistration) &
!          & = newExpectation( & ! mlr todo Expectation --> foundAction -- "Result"
!          &                  newSubject(subName), &
!          &                  nWasCalled() )
   end subroutine registerMockCallBy_subName_

!-old-!   subroutine old_verify(this)
!-old-!      use Exception_mod
!-old-!      class (MockRepository), intent(inout), target :: this
!-old-!      integer iExp, iReg
!-old-!      class (Expectation), pointer :: exp, reg
!-old-!      logical ok
!-old-!      integer nCalls
!-old-!
!-old-!      call throw('MockRepository%verify not implemented.')
!-old-!      return
!-old-!
!-old-!      ! Go through expectation logic. Note:  Maybe use original list of strings approach.
!-old-!      ! Need to work out more complex logic.  Ess. need logic analyzer.
!-old-!      ! Eventually, expectations should probably be trees.
!-old-!
!-old-!      ! Maybe rework the following into "expected vs. found" for greater alignment
!-old-!      ! with existing usage in PFUNIT.  Also consider existing capabilities in PFUNIT.
!-old-!
!-old-!! mlr todo -- verify should not be hardwired for the things it has to handle 
!-old-!! todo -- refactor expectations & foundActionResult -- recall interpreter implementation
!-old-!!
!-old-! 
!-old-!! TODO: move verification to the expectations themselves.
!-old-!! cf. source/ExpectationWasCalled.F90 for a first attempt.
!-old-!! TODO: Improve the expectation list.  Currently a 2 element array!?
!-old-!
!-old-!      do iExp=1,this%lastExpectation  ! 'with' syntax?
!-old-!         exp => this%Expectations(iExp)
!-old-!         ok = .false.
!-old-!
!-old-!!         if(exp%pred%name .eq. 'wasCalled')then
!-old-!!            ok = .false.
!-old-!!            do iReg=1,this%lastRegistration
!-old-!!               reg = this%callRegistry(iReg)
!-old-!!!               print *,'verify1000: ', &
!-old-!!!                    & trim(exp%subj%name)//'='// &
!-old-!!!                    & trim(reg%subj%name)//', '// &
!-old-!!!                    & trim(exp%pred%name)//'='// &
!-old-!!!                    & trim(reg%pred%name)//'.'
!-old-!!               if(exp%subj%name .eq. reg%subj%name) then
!-old-!!                  if(exp%pred%name .eq. reg%pred%name) then
!-old-!!   !???               if(exp%pred .eq. reg%pred) then
!-old-!!                     ok=.true.
!-old-!!                  end if
!-old-!!               end if
!-old-!!            end do
!-old-!!         end if
!-old-!!
!-old-!!         if(exp%pred%name .eq. 'wasNotCalled')then
!-old-!!            ok = .true.
!-old-!!            do iReg=1,this%lastRegistration
!-old-!!               reg = this%callRegistry(iReg)
!-old-!!               if(exp%subj%name .eq. reg%subj%name) then
!-old-!!                  if(trim(reg%pred%name).eq.'wasCalled')then
!-old-!!                     ok = .false.
!-old-!!                  end if
!-old-!!               end if
!-old-!!            end do
!-old-!!         end if
!-old-!
!-old-!         if( &
!-old-!              & (exp%pred%name .eq. 'wasCalled') .or. &
!-old-!              & (exp%pred%name .eq. 'wasCalledOnce') .or. &
!-old-!              & (exp%pred%name .eq. 'wasNotCalled') &
!-old-!              & )then
!-old-!            ok = .true.
!-old-!            nCalls = 0
!-old-!            do iReg=1,this%lastRegistration
!-old-!               reg => this%callRegistry(iReg)
!-old-!               if(exp%subj%name .eq. reg%subj%name) then
!-old-!                  if(trim(reg%pred%name).eq.'wasCalled')then
!-old-!                     nCalls=nCalls+1
!-old-!                  end if
!-old-!               end if
!-old-!            end do
!-old-!            if(exp%pred%name .eq. 'wasCalled')then
!-old-!               ok = nCalls.ge.1
!-old-!            else if(exp%pred%name .eq. 'wasCalledOnce')then
!-old-!               ok = nCalls.eq.1
!-old-!            else if(exp%pred%name .eq. 'wasNotCalled')then
!-old-!               ok = nCalls.eq.0
!-old-!            end if
!-old-!
!-old-!         end if
!-old-!
!-old-!         if(.not.ok)then
!-old-!            call throw('             "'// &
!-old-!                 & trim(exp%subj%name)//'" "'//trim(exp%pred%name)//'" does not hold.')
!-old-!         end if
!-old-!      end do
!-old-!
!-old-!      ! call throw('exception%verify: Not implemented')
!-old-!      
!-old-!    end subroutine old_verify

! TODO: Is this the correct place to keep/use/verify expectations?  Or should we be more like
! exceptions?
!
   subroutine verify(this)
      use Exception_mod
      class (MockRepository), intent(inout), target :: this
      class (Expectation), pointer :: exp, reg
      class (ExpectationPolyWrap), pointer :: expContainer
      type (ExpectationPolyWrapVectorIterator) :: iter
      logical ok
      integer nCalls

      ! call throw('MockRepository%verify not implemented.')
      ! return

      iter = this%expectationList%begin()
      ! If none added, no foul.
      !if (iter == this%expectationList%end()) then
      !   call throw('MockRepository%verify: Error: Empty expectation list!')
      !   return
      !end if

      ! Traverse store of expectations and call on their verification methods.
      ok = .true.
      do while (iter /= this%expectationList%end())
         expContainer => iter%get(); exp => expContainer%get()
         !!! TODO: ADD EVENT LIST HERE (OR SOMEWHERE ELSE...) !!!
         ok = ok .and. exp%verify(this%EventList)
         if (.not.ok) then
            ! .not.ok => an exception was thrown by verify.
            ! Should we retun immediately, or continue through the exceptions?
            return
         end if
         call iter%next()
      end do

      return

!-old-!      ! The old new follows.
!-old-!      
!-old-!      do iExp=1,this%lastExpectation  ! 'with' syntax?
!-old-!         exp => this%Expectations(iExp)
!-old-!         ok = .false.
!-old-!!         ok = exp%verify()
!-old-!      end do

!      call throw('EXCEPTION::MockRepository%verify: Not implemented')

    end subroutine verify

    integer function countTimesCalled(this,subjectName) result(nCalls)
      class (MockRepository), intent(inout), target :: this
      character(*), intent(in) :: subjectName
! multiple      integer                  :: nCalls

      class (Expectation), pointer :: reg

      integer :: iReg
      
      nCalls = 0

      ! iReg is the "found".
      do iReg=1,this%lastRegistration
         reg => this%callRegistry(iReg)
         if(subjectName .eq. reg%subj%name) then
            if(trim(reg%pred%name).eq.'wasCalled')then
               nCalls=nCalls+1
            end if
         end if
      end do

    end function countTimesCalled

   
end module MockRepository_mod
