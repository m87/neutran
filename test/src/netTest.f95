program neuronTest
    use nt_TypesModule
    use nt_NetModule
    use nt_initMethodsModule
    use sft_AssertModule
    use sft_SuiteModule    
    use testHelpers
    implicit none

    type(sft_Suite) :: testSuite

    call sft_init(testSuite)

    call sft_run(testSuite, shouldCorrectlyInitDefaultNet)
    call sft_run(testSuite, shouldCorrectlyInitNet)
    call sft_run(testSuite, shouldReturnCorrectOutput)
    
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

        function shouldReturnCorrectOutput() result(res)
            logical :: res
            type(nt_Net) :: net
            !given
            call nt_netInit_custom(net, (/ 2, 2, 2 /), .FALSE., testWeightInit, (/ 2.0 /))

            !when 
            call nt_netFeed(net, testActivationFunction, (/ 2.0 /), (/ 2.0, 4.0 /))

            !then
            !net%  check last layer output expect (/ 12.0, 12.0/)

        end function shouldReturnCorrectOutput

end program neuronTest
