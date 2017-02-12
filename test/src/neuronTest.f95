program neuronTest
    use nt_TypesModule
    use nt_NeuronModule
    use sft_AssertModule
    use sft_SuiteModule
    implicit none

    type(sft_Suite) :: testSuite

    call sft_init(testSuite)
    
    call sft_run(testSuite, shouldCorrectlyInitHiddenNeuron)

    call sft_summary(testSuite)

    contains

        function shouldCorrectlyInitHiddenNeuron() result(res)
            logical :: res
            type(nt_Neuron) :: neuron
            type(nt_Neuron) :: neuronWithBias

            call nt_hiddenNeuronInit(neuron, 10, .False.)
            call nt_hiddenNuronInit(neuronWithBias, 10, .TRUE.)

            res = sft_assertEqual(size(neuron%synapses), 10) &
                .AND. sft_assertEqual(size(neuronWithBias%synapses), 11)
        end function shouldCorrectlyInitHiddenNeuron

end program neuronTest
