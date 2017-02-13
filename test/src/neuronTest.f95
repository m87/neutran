program neuronTest
    use nt_TypesModule
    use nt_NeuronModule
    use nt_initMethodsModule
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

            call nt_hiddenNeuronInit(neuron, 10, constInit, (/ 10.0 /))

            res = sft_assertEqual(size(neuron%synapses), 10)  & 
                .AND. sft_assertEqual(neuron%synapses(4)%weight, 10.0) 
                
        end function shouldCorrectlyInitHiddenNeuron

end program neuronTest
