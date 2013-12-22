program main
    use types
    implicit none   
    

    integer, dimension(0:2) :: topology
    integer :: i
    type(Net) :: net1
    topology(0)=10
    topology(1)=5
    topology(2)=1
    call init_net(net1,topology,3,0.0,0.0)

stop

end program main
