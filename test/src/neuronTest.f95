program neuronTest
    use nt_TypesModule
    use nt_NeuronModule
    use sft_AssertModule
    use sft_SuiteModule
    implicit none

    type(sft_Suite) :: testSuite

    call sft_init(testSuite)
    
    call sft_run(testSuite, shouldCorrectlyInitNeuron)

    call sft_summary(testSuite)

    contains

        function shouldCorrectlyInitNeuron() result(res)
            logical :: res
            type(nt_Neuron) :: neuron
            type(nt_Neuron) :: neuronWithBias

            call nt_neuronInit(neuron, 10, .False.)
            call nt_neuronInit(neuronWithBias, 10, .TRUE.)

            res = sft_assertEqual(neuron%weightsSize, 10) &
                .AND. sft_assertEqual(neuronWithBias%weightsSize, 11)
        end function shouldCorrectlyInitNeuron

end program neuronTest