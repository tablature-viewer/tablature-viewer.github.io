module AutoscrollSpec where
-- We want to store the autoscroll speed in the URL
-- this means it has to be portable across devices
-- So we need to store it in terms of lines per second and translate that to pixels per second when applying the autoscroll
-- pitfall: bookmarks don't get upated when you change the speed
