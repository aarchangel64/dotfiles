 ;; https://www.reddit.com/r/emacs/comments/920psp/projectile_ignoring_projectile_files/e32u6tk/
 ;; we mainly want projects defined by a few markers and we always want to take the top-most marker.
 ;; Reorder so other cases are secondary
 (after! projectile
   (setq projectile-project-root-files #'( ".projectile" ))
   (setq projectile-project-root-functions #'(projectile-root-top-down
                                              projectile-root-top-down-recurring
                                              projectile-root-bottom-up
                                              projectile-root-local)
   )
 )
