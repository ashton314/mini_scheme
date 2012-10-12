package Analyze;
use strict;
use warnings;

use v5.10;
use Scalar::Util;

sub scheme_analyze {
    my $expr = shift;

    ## REMINDER: NEED TO IMPLEMENT MACRO EXPANSION DURING THIS
    ## PHASE. ADDING SOMETHING TO ANALYZE A MACRO DEFINITION MIGHT BE
    ## NEEDED. ALSO, THERE MUST BE SOME VARIABLE KEPT THAT REMEMBERS
    ## MACRO DEFINITIONS DURING COMPILE TIME SO THEY CAN BE EXPANDED
    ## THERE.
    
    if (ref $expr eq 'ARRAY') {
	given ($$expr[0]) {
	    when ('string') { return sub { $$expr[1]; }; }
	    when ('quote') { return sub { $$expr[1]; }; }
	    when ('set!') {
		my $var = $$expr[1];
		my $val = scheme_analyze($$expr[2]);
		return sub { 
		    my $env = shift;
		    return $$env{$var} = $val->($env);
		}; }
	    when ('define') {}
	    when ('if') {
		my $pred = scheme_analyze($$expr[1]);
		my $tcl  = scheme_analyze($$expr[2]);
		my $fcl  = defined($$expr[3]) ? scheme_analyze($$expr[3]) : 0;
		return sub {
		    my $env = shift;
		    if ($pred->($env) ne 'nil') {
			return $tcl->($env);
		    }
		    else {
			if ($fcl) {
			    return $fcl->($env);
			}
			else {
			    return 'nil';
			}
		    }
		};
	    }
	    when ('lambda') {
		my @expression = @{ $expr };
		my $params = shift @expression;
		my $body = scheme_analyze(['begin', @expression]);

		# WORKING HERE

	    }

	    when ('begin') {}
	    default {}
	}
    }
    else {
	if (looks_like_number($expr)) {
	    return sub {
		return $expr;
	    };
	}
	else {		# Variable
	    return sub {
		my $env = shift;
		return $$env{$expr};
	    };
	}
    }
}



1;
