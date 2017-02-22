program neuronTest
    use nt_TypesModule
    use nt_NetModule
    use nt_initMethodsModule
    use sft_AssertModule
    use sft_SuiteModule    
    implicit none

    type(sft_Suite) :: testSuite

    call sft_init(testSuite)

    call sft_run(testSuite, shouldCorrectlyInitDefaultNet)
    call sft_run(testSuite, shouldCorrectlyInitNet)
    
    call sft_summary(testSuite)

    contains

        function shouldCorrectlyInitDefaultNet() result(res)
            logical :: res

            res = .TRUE.
        end function
        
        function shouldCorrectlyInitNet() result(res)
            logical :: res

            res = .TRUE.
        end function

end program neuronTest
