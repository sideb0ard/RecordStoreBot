#!/usr/bin/perl -w
use strict;

use lib '.';
use Recordstorebot;

my $bot = new Recordstorebot {
	name		=> "Bot",
	scriptfile 	=> "bobbybot.txt",
};


$bot->command_interface();

