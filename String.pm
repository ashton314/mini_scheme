package String;
use overload "" => sub { my $self = shift; $self->{string}; };

sub new {
    my $class = shift;
    my $string = shift;
    return bless { string => $string }, $class;
}

1;
