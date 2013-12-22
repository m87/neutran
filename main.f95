program main
    use types
    implicit none   
    
    integer :: i
    type(layer) :: neu,neu1
    call init_layer(neu,1)
    call init_layer(neu1,1)
   
    write(*,*) neu%neurons(0)%outputWeights%weight
    write(*,*) neu1%neurons(0)%outputWeights%weight
 
    call adaptWeights(neu1%neurons(0), neu)    
stop

end program main
