module TestSuite_mod
   use Test_mod
   implicit none
   private

   public :: TestSuite
   public :: newTestSuite

   type TestReference
      class (Test), allocatable :: pTest
   end type TestReference
   integer, parameter :: MAX_LENGTH_NAME = 32

   type, extends(Test) :: TestSuite
      private
      character(MAX_LENGTH_NAME) :: name
      type (TestReference), allocatable :: tests(:)
   contains
      procedure :: getName => getName2
      procedure :: setName
      procedure :: countTestCases => countTestCases2
      procedure :: run
      procedure :: addTest
      procedure :: getNumTests
      procedure :: copy
      generic :: assignment(=) => copy
      procedure :: getTestCases
   end type TestSuite

   interface newTestSuite
      module procedure newTestSuite_unnamed
      module procedure newTestSuite_named
   end interface newTestSuite

contains

   function newTestSuite_unnamed() result(newSuite)
      type (TestSuite) :: newSuite
      newSuite = newTestSuite_named('')
   end function newTestSuite_unnamed

   function newTestSuite_named(name) result(newSuite)
      type (TestSuite) :: newSuite
      character(len=*), intent(in) :: name

      allocate(newSuite%tests(0))
      call newSuite%setName(name)

   end function newTestSuite_named

   function getName2(this) result(name)
      class (TestSuite), intent(in) :: this
      character(MAX_LENGTH_NAME) :: name
      name = trim(this%name)
   end function getName2

   recursive subroutine copy(this, b)
      class (TestSuite), intent(out) :: this
      type (TestSuite), intent(in) :: b

      integer :: i, n

      call this%setName(b%getName())
      n = b%getNumTests()

      allocate(this%tests(n))
      do i = 1, n
         allocate(this%tests(i)%ptest, source=b%tests(i)%ptest)
      end do

   end subroutine copy

   recursive integer function countTestCases2(this) result(numCases)
      class (TestSuite), intent(in) :: this
      integer :: i
      
      numCases = 0
      do i = 1, this%getNumTests()
         numCases = numCases + this%tests(i)%pTest%countTestCases()
      end do
  
    end function countTestCases2

   recursive subroutine run(this, tstResult, context)
      use ParallelContext_mod
      use TestResult_mod
      class (TestSuite), intent(inout) :: this
      class (TestResult), intent(inout) :: tstResult
      class (ParallelContext), intent(in) :: context

      integer :: i
      
      do i = 1, this%getNumTests()
         call this%tests(i)%ptest%run(tstResult, context)
      end do
      
   end subroutine run
   
   recursive subroutine addTest(this, aTest)
      class (TestSuite), intent(inout) :: this
      class (Test), intent(in) :: aTest
      
      call extend(this%tests)
      allocate(this%tests(this%getNumTests())%pTest, source=aTest)
      
   contains   
      
      recursive subroutine extend(list)
         type (TestReference), allocatable :: list(:)
         type (TestReference), allocatable :: temp(:)
         integer :: i, n
         
         n = size(list)
         call move_alloc(from=list, to=temp)

         allocate(list(n+1))
         do i = 1, n
            call kludge_move_alloc(from=temp(i)%ptest, to=list(i)%ptest)
         end do

         deallocate(temp)

      end subroutine extend

      subroutine kludge_move_alloc(from, to)
         class (Test), allocatable :: from
         class (Test), allocatable :: to
         call move_alloc(from=from, to=to)
      end subroutine kludge_move_alloc
      
   end subroutine addTest
   
   pure integer function getNumTests(this) 
      class (TestSuite), intent(in) :: this
      getNumTests = size(this%tests)
   end function getNumTests

   subroutine setName(this, name)
      class (TestSuite), intent(inout) :: this
      character(len=*),intent(in) :: name
      this%name = trim(name)
   end subroutine setName

#if defined(__INTEL_COMPILER) && (INTEL_13)
   recursive function getTestCases(this) result(testList)
      use Exception_mod
      use Test_mod
      use TestCase_mod
      class (TestSuite), intent(in) :: this
      type (TestCaseReference), allocatable :: testList(:)
      type (TestCaseReference), allocatable :: tmp(:)

      integer :: i, j
      integer :: n, m

      allocate(testList(this%countTestCases2()))

      n = 1
      do i = 1, size(this%tests)
         associate (t => this%tests(i)%pTest)
           select type (t)
           class is (TestCase)
              ! ifort 13.1 cannot handle direct assignment of polymorphic here
              allocate(testList(n)%test, source=t)
!!$              testList(n) = TestCaseReference(t)
              n = n + 1
           class is (TestSuite)
              m = t%countTestCases()
              ! ifort 13.1 is incorrectly handling assignment into subrange of tmpList
              ! It reallocates the array and gets the wrong size as a result.
              ! Forced to do explict loop over a temporary.
              tmp = t%getTestCases()
              do j = 1, m
                 allocate(testList(n+j-1)%test, source=tmp(j)%test)
              end do
!!$              testList(n:n+m-1) = t%getTestCases()
              n = n + m
           class default
              call throw('Unsupportes Test subclass in TestSuite::getTestCases()')
           end select
         end associate
      end do

   end function getTestCases
#else
   subroutine  getTestCases(this, testList)
      use Exception_mod
      use Test_mod
      use TestCase_mod
      class (TestSuite), intent(in) :: this
      type (TestCaseReference), allocatable :: testList(:)

      integer :: n

      n = countTestCases2(this)
      allocate(testList(n))
      
      n = 0
      call accumulateTestCases(this, testList, n)

   contains
      
      recursive subroutine accumulateTestCases(this, testList, n)
         class (TestSuite), target, intent(in) :: this
         type (TestCaseReference), intent(inout) :: testList(:)
         integer, intent(inout) :: n
         
         integer :: i, j
         class (Test), pointer :: ref

         do i = 1, size(this%tests)

           ref => this%tests(i)%pTest
            associate (t => ref)

               select type (t)
               class is (TestCase)
                  n = n + 1
                  allocate(testList(n)%test, source=t)
               class is (TestSuite)
                  call accumulateTestCases(t, testList, n)
               class default
                  call throw('Unsupportes Test subclass in TestSuite::getTestCases()')
               end select
             end associate
          end do

       end subroutine accumulateTestCases

    end subroutine getTestCases
#endif

end module TestSuite_mod
