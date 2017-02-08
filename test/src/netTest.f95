program netTest
    use nt_TypesModule
    use nt_NetModule
    use sft_AssertModule
    use sft_SuiteModule
    implicit none

    type(sft_Suite) :: testSuite

    call sft_init(testSuite)
    
    call sft_run(testSuite, shouldCorrectlyInitNet)

    call sft_summary(testSuite)

    contains

        function shouldCorrectlyInitNet() result(res)
            logical :: res
            type(nt_Net) :: net

            call nt_netInit(net, (/ 1, 2, 3 /))

            res = sft_assertEqual(net%topology(0), 1) &
                .AND. sft_assertEqual(net%topology(1), 2) &
                .AND. sft_assertEqual(net%topology(1), 3)
        end function shouldCorrectlyInitNet




end program netTest
