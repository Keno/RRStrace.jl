import RRStrace
using Cxx
sess = icxx"""rr::ReplaySession::create("");"""
RRStrace.strace(sess)
